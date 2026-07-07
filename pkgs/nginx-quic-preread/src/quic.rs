//! Extract the TLS ClientHello SNI (RFC 6066) and ALPN (RFC 7301) from a QUIC
//! Initial packet — the QUIC/UDP analogue of nginx's `ssl_preread` for TCP.
//!
//! A QUIC Initial packet carries the TLS ClientHello inside CRYPTO frames, but
//! the packet is *protected* (encrypted + authenticated). Crucially, Initial
//! packets are protected with keys derived deterministically from the
//! **Destination Connection ID** using a per-version salt (RFC 9001 §5.2), so
//! an on-path element can decrypt them *without* the server's private key. That
//! is exactly what makes SNI-based routing of QUIC possible.
//!
//! Pipeline (all pure Rust, no nginx dependency — see the tests at the bottom):
//!
//! 1. Parse the QUIC long header, recover version + Destination Connection ID.
//! 2. Derive the client Initial `key` / `iv` / `hp` (RFC 9001 §5.2).
//! 3. Remove header protection to learn the packet-number length (RFC 9001 §5.4).
//! 4. AEAD-decrypt the payload with AES-128-GCM (RFC 9001 §5.3).
//! 5. Reassemble CRYPTO frames into the TLS handshake stream (RFC 9000 §19.6).
//! 6. Parse the ClientHello and pull out `server_name` + `application_layer_
//!    protocol_negotiation` (RFC 8446 §4).
//!
//! Only the pieces needed to reach the SNI/ALPN extensions are implemented;
//! anything unrecognised makes the relevant step bail out cleanly rather than
//! guess.

use aes::cipher::generic_array::GenericArray;
use aes::cipher::{BlockEncrypt, KeyInit as _};
use aes::Aes128;
use aes_gcm::aead::Aead;
use aes_gcm::{Aes128Gcm, Nonce};
use hkdf::Hkdf;
use sha2::Sha256;

/// What we managed to read out of the ClientHello.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct PrereadInfo {
    /// SNI host name (`server_name`, `host_name` type), if present.
    pub server_name: Option<String>,
    /// ALPN protocol identifiers, in offered order.
    pub alpn: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrereadError {
    /// Not a QUIC long-header Initial packet (short header, wrong type, garbage).
    NotQuicInitial,
    /// QUIC version whose Initial salt/labels we don't know.
    UnsupportedVersion(u32),
    /// Buffer ended in the middle of a field we needed.
    Truncated,
    /// AEAD tag did not verify — wrong keys, or not really an Initial packet.
    DecryptFailed,
    /// Structurally decrypted but the ClientHello could not be parsed.
    ParseError,
}

// ---------------------------------------------------------------------------
// QUIC versions we understand (Initial salts differ per version).
// ---------------------------------------------------------------------------

/// RFC 9001 §5.2 — Initial salt for QUIC v1.
const INITIAL_SALT_V1: [u8; 20] = [
    0x38, 0x76, 0x2c, 0xf7, 0xf5, 0x59, 0x34, 0xb3, 0x4d, 0x17, 0x9a, 0xe6, 0xa4, 0xc8, 0x0c, 0xad,
    0xcc, 0xbb, 0x7f, 0x0a,
];

/// RFC 9369 §3.3.1 — Initial salt for QUIC v2.
const INITIAL_SALT_V2: [u8; 20] = [
    0x0d, 0xed, 0xe3, 0xde, 0xf7, 0x00, 0xa6, 0xdb, 0x81, 0x93, 0x81, 0xbe, 0x6e, 0x26, 0x9d, 0xcb,
    0xf9, 0xbd, 0x2e, 0xd9,
];

const VERSION_V1: u32 = 0x0000_0001;
const VERSION_V2: u32 = 0x6b33_43cf;

struct VersionParams {
    salt: &'static [u8; 20],
    /// HKDF-Expand-Label labels differ between v1 ("quic ...") and v2 ("quicv2 ...").
    key_label: &'static str,
    iv_label: &'static str,
    hp_label: &'static str,
    /// Long-header packet-type bits (RFC 9000 §17.2 / RFC 9369 §3.2) that mark
    /// an Initial packet, already shifted into the low two bits.
    initial_type: u8,
}

fn version_params(version: u32) -> Option<VersionParams> {
    match version {
        VERSION_V1 => Some(VersionParams {
            salt: &INITIAL_SALT_V1,
            key_label: "quic key",
            iv_label: "quic iv",
            hp_label: "quic hp",
            initial_type: 0b00,
        }),
        VERSION_V2 => Some(VersionParams {
            salt: &INITIAL_SALT_V2,
            key_label: "quicv2 key",
            iv_label: "quicv2 iv",
            hp_label: "quicv2 hp",
            initial_type: 0b01,
        }),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Key schedule (RFC 8446 §7.1 HKDF-Expand-Label, RFC 9001 §5.2).
// ---------------------------------------------------------------------------

/// TLS 1.3 HKDF-Expand-Label with an empty context, over SHA-256.
fn hkdf_expand_label(secret: &[u8], label: &str, length: usize) -> Vec<u8> {
    // struct {
    //   uint16 length;
    //   opaque label<7..255> = "tls13 " + Label;
    //   opaque context<0..255> = "";
    // } HkdfLabel;
    let mut full_label = Vec::with_capacity(6 + label.len());
    full_label.extend_from_slice(b"tls13 ");
    full_label.extend_from_slice(label.as_bytes());

    let mut info = Vec::with_capacity(2 + 1 + full_label.len() + 1);
    info.extend_from_slice(&(length as u16).to_be_bytes());
    info.push(full_label.len() as u8);
    info.extend_from_slice(&full_label);
    info.push(0); // zero-length context

    let hk = Hkdf::<Sha256>::from_prk(secret).expect("PRK is a full SHA-256 block");
    let mut out = vec![0u8; length];
    hk.expand(&info, &mut out)
        .expect("valid HKDF output length");
    out
}

struct InitialKeys {
    key: [u8; 16],
    iv: [u8; 12],
    hp: [u8; 16],
}

/// Derive the *client* Initial protection keys from the Destination Connection
/// ID (RFC 9001 §5.2). We only ever need the client side — the ClientHello
/// travels client → server.
fn derive_client_initial_keys(dcid: &[u8], params: &VersionParams) -> InitialKeys {
    // initial_secret = HKDF-Extract(initial_salt, client_dst_connection_id)
    let (initial_secret, _) = Hkdf::<Sha256>::extract(Some(params.salt), dcid);
    // client_initial_secret = HKDF-Expand-Label(initial_secret, "client in", "", 32)
    let client_secret = hkdf_expand_label(initial_secret.as_slice(), "client in", 32);

    let key = hkdf_expand_label(&client_secret, params.key_label, 16);
    let iv = hkdf_expand_label(&client_secret, params.iv_label, 12);
    let hp = hkdf_expand_label(&client_secret, params.hp_label, 16);

    InitialKeys {
        key: key.try_into().unwrap(),
        iv: iv.try_into().unwrap(),
        hp: hp.try_into().unwrap(),
    }
}

// ---------------------------------------------------------------------------
// QUIC variable-length integers (RFC 9000 §16).
// ---------------------------------------------------------------------------

/// Read a QUIC varint at `*pos`, advancing `*pos` past it.
fn read_varint(buf: &[u8], pos: &mut usize) -> Option<u64> {
    let first = *buf.get(*pos)?;
    let len = 1usize << (first >> 6); // top two bits select 1/2/4/8 bytes
    if *pos + len > buf.len() {
        return None;
    }
    let mut val = (first & 0x3f) as u64;
    for i in 1..len {
        val = (val << 8) | buf[*pos + i] as u64;
    }
    *pos += len;
    Some(val)
}

// ---------------------------------------------------------------------------
// Long-header parse + de-protection (RFC 9000 §17.2, RFC 9001 §5.3–5.4).
// ---------------------------------------------------------------------------

/// AES-128 single-block "encrypt in place", used for the header-protection mask
/// and (in tests) to build a sample. This is ECB over exactly one 16-byte block.
fn aes128_ecb_block(key: &[u8; 16], block: &[u8; 16]) -> [u8; 16] {
    let cipher = Aes128::new(GenericArray::from_slice(key));
    let mut b = *block;
    cipher.encrypt_block(GenericArray::from_mut_slice(&mut b));
    b
}

/// Decrypt a QUIC Initial packet's payload and return the plaintext QUIC frames.
///
/// `datagram` is a full UDP payload; the Initial packet is expected first (it
/// may be followed by coalesced packets which we ignore).
fn decrypt_initial_payload(datagram: &[u8]) -> Result<Vec<u8>, PrereadError> {
    // --- Long header, fixed prefix ---------------------------------------
    // First byte: 1 header-form | 1 fixed | 2 type | 4 version-specific(protected)
    let first = *datagram.first().ok_or(PrereadError::Truncated)?;
    if first & 0x80 == 0 {
        // Short header -> not an Initial.
        return Err(PrereadError::NotQuicInitial);
    }

    if datagram.len() < 7 {
        return Err(PrereadError::Truncated);
    }
    let version = u32::from_be_bytes([datagram[1], datagram[2], datagram[3], datagram[4]]);
    let params = version_params(version).ok_or(PrereadError::UnsupportedVersion(version))?;

    // Long-header packet type lives in bits 0x30 of the first byte.
    if (first >> 4) & 0b11 != params.initial_type {
        return Err(PrereadError::NotQuicInitial);
    }

    let mut pos = 5usize;
    // Destination Connection ID.
    let dcid_len = *datagram.get(pos).ok_or(PrereadError::Truncated)? as usize;
    pos += 1;
    if dcid_len > 20 || pos + dcid_len > datagram.len() {
        return Err(PrereadError::Truncated);
    }
    let dcid = &datagram[pos..pos + dcid_len];
    pos += dcid_len;
    // Source Connection ID.
    let scid_len = *datagram.get(pos).ok_or(PrereadError::Truncated)? as usize;
    pos += 1;
    if scid_len > 20 || pos + scid_len > datagram.len() {
        return Err(PrereadError::Truncated);
    }
    pos += scid_len;
    // Token (Initial only).
    let token_len = read_varint(datagram, &mut pos).ok_or(PrereadError::Truncated)? as usize;
    if pos + token_len > datagram.len() {
        return Err(PrereadError::Truncated);
    }
    pos += token_len;
    // Length: number of bytes in packet-number + protected payload.
    let length = read_varint(datagram, &mut pos).ok_or(PrereadError::Truncated)? as usize;

    // `pos` now points at the (protected) packet number. Everything from here
    // to pn_offset+length is the Initial packet body.
    let pn_offset = pos;
    let packet_end = pn_offset
        .checked_add(length)
        .filter(|&e| e <= datagram.len())
        .ok_or(PrereadError::Truncated)?;

    // --- Derive keys & remove header protection (RFC 9001 §5.4) ----------
    let keys = derive_client_initial_keys(dcid, &params);

    // Sample starts 4 bytes into the packet-number field (max PN length).
    let sample_offset = pn_offset + 4;
    if sample_offset + 16 > datagram.len() {
        return Err(PrereadError::Truncated);
    }
    let sample: [u8; 16] = datagram[sample_offset..sample_offset + 16]
        .try_into()
        .unwrap();
    let mask = aes128_ecb_block(&keys.hp, &sample);

    // Work on a mutable copy of the (still-protected) header + body.
    let mut pkt = datagram[..packet_end].to_vec();

    // Unmask the low 4 bits of the first byte (long header).
    pkt[0] ^= mask[0] & 0x0f;
    let pn_len = ((pkt[0] & 0x03) + 1) as usize;
    if pn_offset + pn_len > pkt.len() {
        return Err(PrereadError::Truncated);
    }
    // Unmask the packet-number bytes and read the truncated packet number.
    let mut packet_number: u64 = 0;
    for i in 0..pn_len {
        pkt[pn_offset + i] ^= mask[1 + i];
        packet_number = (packet_number << 8) | pkt[pn_offset + i] as u64;
    }

    // --- AEAD decrypt (RFC 9001 §5.3) ------------------------------------
    // Associated data = the packet header, up to and including the packet number.
    let header_len = pn_offset + pn_len;
    let aad = pkt[..header_len].to_vec();
    let ciphertext = &pkt[header_len..]; // protected payload incl. 16-byte tag

    // Nonce = IV XOR left-padded packet number.
    let mut nonce = keys.iv;
    let pn_bytes = packet_number.to_be_bytes(); // 8 bytes, big-endian
    for i in 0..8 {
        nonce[12 - 8 + i] ^= pn_bytes[i];
    }

    let cipher = Aes128Gcm::new(GenericArray::from_slice(&keys.key));
    let plaintext = cipher
        .decrypt(
            Nonce::from_slice(&nonce),
            aes_gcm::aead::Payload {
                msg: ciphertext,
                aad: &aad,
            },
        )
        .map_err(|_| PrereadError::DecryptFailed)?;

    Ok(plaintext)
}

// ---------------------------------------------------------------------------
// CRYPTO frame reassembly (RFC 9000 §19).
// ---------------------------------------------------------------------------

/// Walk the decrypted QUIC frames and reassemble the contiguous prefix of the
/// CRYPTO stream (offset 0 onward). Returns whatever contiguous bytes we have —
/// the SNI lives near the front of the ClientHello, so the first Initial packet
/// almost always contains it even when the full ClientHello spans several.
fn reassemble_crypto(plaintext: &[u8]) -> Result<Vec<u8>, PrereadError> {
    let mut segments: Vec<(u64, &[u8])> = Vec::new();
    let mut pos = 0usize;

    while pos < plaintext.len() {
        let frame_type = read_varint(plaintext, &mut pos).ok_or(PrereadError::ParseError)?;
        match frame_type {
            0x00 => {} // PADDING (a run of zero bytes; each is its own "frame")
            0x01 => {} // PING
            0x02 | 0x03 => {
                // ACK: largest, delay, range_count, first_range, {gap, len}*, [ecn].
                let _largest = read_varint(plaintext, &mut pos).ok_or(PrereadError::ParseError)?;
                let _delay = read_varint(plaintext, &mut pos).ok_or(PrereadError::ParseError)?;
                let range_count =
                    read_varint(plaintext, &mut pos).ok_or(PrereadError::ParseError)?;
                let _first = read_varint(plaintext, &mut pos).ok_or(PrereadError::ParseError)?;
                for _ in 0..range_count {
                    let _gap = read_varint(plaintext, &mut pos).ok_or(PrereadError::ParseError)?;
                    let _len = read_varint(plaintext, &mut pos).ok_or(PrereadError::ParseError)?;
                }
                if frame_type == 0x03 {
                    for _ in 0..3 {
                        read_varint(plaintext, &mut pos).ok_or(PrereadError::ParseError)?;
                    }
                }
            }
            0x06 => {
                // CRYPTO: offset, length, data.
                let offset = read_varint(plaintext, &mut pos).ok_or(PrereadError::ParseError)?;
                let len =
                    read_varint(plaintext, &mut pos).ok_or(PrereadError::ParseError)? as usize;
                if pos + len > plaintext.len() {
                    return Err(PrereadError::ParseError);
                }
                segments.push((offset, &plaintext[pos..pos + len]));
                pos += len;
            }
            _ => {
                // Any other frame type in an Initial (e.g. CONNECTION_CLOSE) means
                // there is no more CRYPTO to gather from here.
                break;
            }
        }
    }

    // Stitch the contiguous run starting at offset 0.
    segments.sort_by_key(|(off, _)| *off);
    let mut out: Vec<u8> = Vec::new();
    for (off, data) in segments {
        let off = off as usize;
        if off > out.len() {
            break; // gap — stop at the contiguous prefix
        }
        if off + data.len() > out.len() {
            out.extend_from_slice(&data[(out.len() - off)..]);
        }
    }
    Ok(out)
}

// ---------------------------------------------------------------------------
// TLS ClientHello parsing (RFC 8446 §4, RFC 6066, RFC 7301).
// ---------------------------------------------------------------------------

/// Cursor helpers over the handshake bytes; every read is bounds-checked and
/// short buffers simply yield `None`, which the caller turns into "no result"
/// rather than an error (the ClientHello may be legitimately incomplete).
struct Reader<'a> {
    buf: &'a [u8],
    pos: usize,
}

impl<'a> Reader<'a> {
    fn new(buf: &'a [u8]) -> Self {
        Reader { buf, pos: 0 }
    }
    fn take(&mut self, n: usize) -> Option<&'a [u8]> {
        let s = self.buf.get(self.pos..self.pos + n)?;
        self.pos += n;
        Some(s)
    }
    fn u8(&mut self) -> Option<u8> {
        Some(self.take(1)?[0])
    }
    fn u16(&mut self) -> Option<usize> {
        let b = self.take(2)?;
        Some(((b[0] as usize) << 8) | b[1] as usize)
    }
    fn u24(&mut self) -> Option<usize> {
        let b = self.take(3)?;
        Some(((b[0] as usize) << 16) | ((b[1] as usize) << 8) | b[2] as usize)
    }
    /// Read a vector prefixed by an 8-bit length and skip its contents.
    fn skip_u8_vec(&mut self) -> Option<()> {
        let n = self.u8()? as usize;
        self.take(n).map(|_| ())
    }
    /// Read a vector prefixed by a 16-bit length and skip its contents.
    fn skip_u16_vec(&mut self) -> Option<()> {
        let n = self.u16()?;
        self.take(n).map(|_| ())
    }
}

/// Parse a ClientHello handshake message and extract SNI + ALPN.
fn parse_client_hello(handshake: &[u8]) -> Result<PrereadInfo, PrereadError> {
    let mut r = Reader::new(handshake);
    let msg_type = r.u8().ok_or(PrereadError::Truncated)?;
    if msg_type != 0x01 {
        return Err(PrereadError::ParseError); // not a ClientHello
    }
    // 3-byte handshake length (we don't require the whole body to be present).
    let _len = r.u24().ok_or(PrereadError::Truncated)?;

    // legacy_version(2) + random(32)
    r.take(2).ok_or(PrereadError::Truncated)?;
    r.take(32).ok_or(PrereadError::Truncated)?;
    // legacy_session_id, cipher_suites, legacy_compression_methods
    r.skip_u8_vec().ok_or(PrereadError::Truncated)?;
    r.skip_u16_vec().ok_or(PrereadError::Truncated)?;
    r.skip_u8_vec().ok_or(PrereadError::Truncated)?;

    // extensions<2 length>
    let ext_total = r.u16().ok_or(PrereadError::Truncated)?;
    let ext_end = (r.pos + ext_total).min(r.buf.len());

    let mut info = PrereadInfo::default();
    while r.pos + 4 <= ext_end {
        let ext_type = r.u16().ok_or(PrereadError::Truncated)?;
        let ext_len = r.u16().ok_or(PrereadError::Truncated)?;
        let body = match r.take(ext_len) {
            Some(b) => b,
            None => break, // extension truncated by an incomplete ClientHello
        };
        match ext_type {
            0x0000 => info.server_name = parse_sni(body),
            0x0010 => info.alpn = parse_alpn(body),
            _ => {}
        }
    }

    Ok(info)
}

/// server_name extension (RFC 6066 §3): ServerNameList of {name_type, HostName}.
fn parse_sni(body: &[u8]) -> Option<String> {
    let mut r = Reader::new(body);
    let list_len = r.u16()?;
    let list_end = (r.pos + list_len).min(r.buf.len());
    while r.pos + 3 <= list_end {
        let name_type = r.u8()?;
        let name = {
            let n = r.u16()?;
            r.take(n)?
        };
        if name_type == 0 {
            // host_name — must be ASCII per spec; be lenient and lossy-decode.
            return Some(String::from_utf8_lossy(name).into_owned());
        }
    }
    None
}

/// ALPN extension (RFC 7301 §3.1): ProtocolNameList of 8-bit-length strings.
fn parse_alpn(body: &[u8]) -> Vec<String> {
    let mut out = Vec::new();
    let mut r = Reader::new(body);
    let list_len = match r.u16() {
        Some(n) => n,
        None => return out,
    };
    let list_end = (r.pos + list_len).min(r.buf.len());
    while r.pos < list_end {
        let n = match r.u8() {
            Some(n) => n as usize,
            None => break,
        };
        match r.take(n) {
            Some(p) => out.push(String::from_utf8_lossy(p).into_owned()),
            None => break,
        }
    }
    out
}

// ---------------------------------------------------------------------------
// Public entry point.
// ---------------------------------------------------------------------------

/// Decrypt a QUIC Initial datagram and extract the ClientHello SNI + ALPN.
///
/// Returns `Ok(PrereadInfo)` when the packet was a decryptable Initial and the
/// ClientHello parsed; `server_name`/`alpn` may still be empty if the client
/// didn't send them. Returns an `Err` for non-Initial / undecryptable / garbled
/// input so the caller can distinguish "route by default" from "keep buffering".
pub fn quic_preread(datagram: &[u8]) -> Result<PrereadInfo, PrereadError> {
    let plaintext = decrypt_initial_payload(datagram)?;
    let crypto = reassemble_crypto(&plaintext)?;
    if crypto.is_empty() {
        return Err(PrereadError::Truncated);
    }
    parse_client_hello(&crypto)
}

// ===========================================================================
// Tests
// ===========================================================================
#[cfg(test)]
mod tests {
    use super::*;

    fn unhex(s: &str) -> Vec<u8> {
        let s: String = s.chars().filter(|c| !c.is_whitespace()).collect();
        (0..s.len())
            .step_by(2)
            .map(|i| u8::from_str_radix(&s[i..i + 2], 16).unwrap())
            .collect()
    }

    /// RFC 9001 Appendix A.1 — the authoritative worked example. Validates the
    /// Initial salt, HKDF-Extract, HKDF-Expand-Label and every label string
    /// against the values published in the RFC.
    #[test]
    fn rfc9001_key_derivation() {
        let dcid = unhex("8394c8f03e515708");
        let params = version_params(VERSION_V1).unwrap();

        let (initial_secret, _) = Hkdf::<Sha256>::extract(Some(params.salt), &dcid);
        assert_eq!(
            initial_secret.as_slice(),
            unhex("7db5df06e7a69e432496adedb00851923595221596ae2ae9fb8115c1e9ed0a44").as_slice(),
        );

        // key / iv / hp are the canonical published RFC 9001 A.1 values.
        let keys = derive_client_initial_keys(&dcid, &params);
        assert_eq!(
            keys.key,
            unhex("1f369613dd76d5467730efcbe3b1a22d").as_slice()
        );
        assert_eq!(keys.iv, unhex("fa044b2f42a3fd3b46fb255c").as_slice());
        assert_eq!(
            keys.hp,
            unhex("9f50449e04a0e810283a1e9933adedd2").as_slice()
        );
    }

    /// RFC 9369 Appendix A.4 — QUIC v2 key derivation for the same DCID.
    #[test]
    fn rfc9369_v2_key_derivation() {
        let dcid = unhex("8394c8f03e515708");
        let params = version_params(VERSION_V2).unwrap();
        let keys = derive_client_initial_keys(&dcid, &params);
        assert_eq!(
            keys.key,
            unhex("8b1a0bc121284290a29e0971b5cd045d").as_slice()
        );
        assert_eq!(keys.iv, unhex("91f73e2351d8fa91660e909f").as_slice());
        assert_eq!(
            keys.hp,
            unhex("45b95e15235d6f45a6b19cbcb0294ba9").as_slice()
        );
    }

    /// Hand-built ClientHello plaintext: isolates the frame-reassembly and TLS
    /// parsing from any crypto.
    #[test]
    fn parse_clienthello_from_frames() {
        let crypto = build_client_hello("preread.example.com", &["h3", "h3-29"]);

        // Wrap it in a CRYPTO frame at offset 0, preceded by a PADDING byte and
        // a PING, to exercise the frame walker.
        let mut frames = vec![0x00, 0x01]; // PADDING, PING
        frames.push(0x06); // CRYPTO
        frames.push(0x00); // offset varint = 0
        push_varint(&mut frames, crypto.len() as u64);
        frames.extend_from_slice(&crypto);

        let reassembled = reassemble_crypto(&frames).unwrap();
        let info = parse_client_hello(&reassembled).unwrap();
        assert_eq!(info.server_name.as_deref(), Some("preread.example.com"));
        assert_eq!(info.alpn, vec!["h3".to_string(), "h3-29".to_string()]);
    }

    /// Split the ClientHello across two out-of-order CRYPTO frames to exercise
    /// reassembly.
    #[test]
    fn reassembles_split_crypto_frames() {
        let crypto = build_client_hello("split.example", &["h3"]);
        let (a, b) = crypto.split_at(crypto.len() / 2);

        let mut frames = Vec::new();
        // second half first, to check sorting by offset
        frames.push(0x06);
        push_varint(&mut frames, (crypto.len() / 2) as u64);
        push_varint(&mut frames, b.len() as u64);
        frames.extend_from_slice(b);
        // first half
        frames.push(0x06);
        push_varint(&mut frames, 0);
        push_varint(&mut frames, a.len() as u64);
        frames.extend_from_slice(a);

        let reassembled = reassemble_crypto(&frames).unwrap();
        let info = parse_client_hello(&reassembled).unwrap();
        assert_eq!(info.server_name.as_deref(), Some("split.example"));
    }

    /// Full end-to-end: encrypt a crafted Initial with the RFC-validated client
    /// keys, then run the public `quic_preread` decrypt+parse pipeline over it.
    /// Exercises header-protection offsets, the AEAD nonce/AAD construction, and
    /// the packet framing exactly as they appear on the wire.
    #[test]
    fn end_to_end_encrypt_then_preread() {
        for version in [VERSION_V1, VERSION_V2] {
            let crypto = build_client_hello("roundtrip.thalheim.io", &["h3"]);
            let datagram = build_initial_packet(version, &crypto);

            let info = quic_preread(&datagram)
                .unwrap_or_else(|e| panic!("preread failed for version {version:#x}: {e:?}"));
            assert_eq!(
                info.server_name.as_deref(),
                Some("roundtrip.thalheim.io"),
                "version {version:#x}"
            );
            assert_eq!(info.alpn, vec!["h3".to_string()], "version {version:#x}");
        }
    }

    #[test]
    fn rejects_short_header() {
        // 0x40: header form bit clear -> short header.
        assert_eq!(
            quic_preread(&[0x40, 0, 0, 0]),
            Err(PrereadError::NotQuicInitial)
        );
    }

    #[test]
    fn rejects_unknown_version() {
        // Long-header Initial-looking first byte, bogus version.
        let mut d = vec![0xc3, 0xde, 0xad, 0xbe, 0xef, 0x00, 0x00];
        d.extend(std::iter::repeat(0).take(1200));
        assert!(matches!(
            quic_preread(&d),
            Err(PrereadError::UnsupportedVersion(0xdeadbeef))
        ));
    }

    // --- test helpers ----------------------------------------------------

    fn push_varint(buf: &mut Vec<u8>, v: u64) {
        // Encode with the shortest form (sufficient for test sizes).
        if v < 64 {
            buf.push(v as u8);
        } else if v < 16384 {
            buf.extend_from_slice(&((v as u16) | 0x4000).to_be_bytes());
        } else {
            buf.extend_from_slice(&((v as u32) | 0x8000_0000).to_be_bytes());
        }
    }

    /// Minimal but well-formed TLS 1.3 ClientHello carrying SNI + ALPN.
    fn build_client_hello(sni: &str, alpn: &[&str]) -> Vec<u8> {
        // --- server_name extension ---
        let mut sni_ext = Vec::new();
        let host = sni.as_bytes();
        let mut list = vec![0x00]; // name_type host_name
        list.extend_from_slice(&(host.len() as u16).to_be_bytes());
        list.extend_from_slice(host);
        sni_ext.extend_from_slice(&(list.len() as u16).to_be_bytes());
        sni_ext.extend_from_slice(&list);

        // --- ALPN extension ---
        let mut alpn_list = Vec::new();
        for p in alpn {
            alpn_list.push(p.len() as u8);
            alpn_list.extend_from_slice(p.as_bytes());
        }
        let mut alpn_ext = Vec::new();
        alpn_ext.extend_from_slice(&(alpn_list.len() as u16).to_be_bytes());
        alpn_ext.extend_from_slice(&alpn_list);

        // --- extensions block ---
        let mut exts = Vec::new();
        exts.extend_from_slice(&0x0000u16.to_be_bytes());
        exts.extend_from_slice(&(sni_ext.len() as u16).to_be_bytes());
        exts.extend_from_slice(&sni_ext);
        exts.extend_from_slice(&0x0010u16.to_be_bytes());
        exts.extend_from_slice(&(alpn_ext.len() as u16).to_be_bytes());
        exts.extend_from_slice(&alpn_ext);

        // --- ClientHello body ---
        let mut body = Vec::new();
        body.extend_from_slice(&0x0303u16.to_be_bytes()); // legacy_version TLS1.2
        body.extend_from_slice(&[0x42; 32]); // random
        body.push(0x00); // session id len
        body.extend_from_slice(&0x0002u16.to_be_bytes()); // cipher suites len
        body.extend_from_slice(&0x1301u16.to_be_bytes()); // TLS_AES_128_GCM_SHA256
        body.push(0x01); // compression methods len
        body.push(0x00); // null compression
        body.extend_from_slice(&(exts.len() as u16).to_be_bytes());
        body.extend_from_slice(&exts);

        // --- handshake framing ---
        let mut hs = Vec::new();
        hs.push(0x01); // ClientHello
        let l = body.len();
        hs.extend_from_slice(&[(l >> 16) as u8, (l >> 8) as u8, l as u8]);
        hs.extend_from_slice(&body);
        hs
    }

    /// Encrypt `crypto` (a TLS handshake) into a wire-format QUIC Initial packet
    /// for the given version, mirroring RFC 9001 §5.3–5.4 on the sender side.
    fn build_initial_packet(version: u32, crypto: &[u8]) -> Vec<u8> {
        let params = version_params(version).unwrap();
        let dcid = unhex("8394c8f03e515708");
        let keys = derive_client_initial_keys(&dcid, &params);

        // CRYPTO frame at offset 0 carrying the ClientHello.
        let mut payload = vec![0x06];
        push_varint(&mut payload, 0);
        push_varint(&mut payload, crypto.len() as u64);
        payload.extend_from_slice(crypto);
        // Pad so the Initial is >= 1200 bytes (and long enough for HP sampling).
        if payload.len() < 1180 {
            payload.resize(1180, 0x00); // trailing PADDING frames
        }

        let packet_number: u32 = 0;
        let pn_len = 4usize; // encode PN in 4 bytes to match sampling assumptions
        let first_type = params.initial_type << 4;
        // first byte: long(1) fixed(1) type(2) reserved(2)=0 pn_len(2)=pn_len-1
        let first = 0x80 | 0x40 | first_type | ((pn_len - 1) as u8);

        // Build the unprotected header.
        let mut header = Vec::new();
        header.push(first);
        header.extend_from_slice(&version.to_be_bytes());
        header.push(dcid.len() as u8);
        header.extend_from_slice(&dcid);
        header.push(0x00); // zero-length SCID
        push_varint(&mut header, 0); // zero-length token
        let length = pn_len + payload.len() + 16; // pn + payload + AEAD tag
        push_varint(&mut header, length as u64);
        let pn_offset = header.len();
        header.extend_from_slice(&packet_number.to_be_bytes()); // 4-byte PN

        // AEAD encrypt.
        let mut nonce = keys.iv;
        let pn_bytes = (packet_number as u64).to_be_bytes();
        for i in 0..8 {
            nonce[12 - 8 + i] ^= pn_bytes[i];
        }
        let cipher = Aes128Gcm::new(GenericArray::from_slice(&keys.key));
        let ct = cipher
            .encrypt(
                Nonce::from_slice(&nonce),
                aes_gcm::aead::Payload {
                    msg: &payload,
                    aad: &header,
                },
            )
            .unwrap();

        let mut packet = header.clone();
        packet.extend_from_slice(&ct);

        // Apply header protection.
        let sample_offset = pn_offset + 4;
        let sample: [u8; 16] = packet[sample_offset..sample_offset + 16]
            .try_into()
            .unwrap();
        let mask = aes128_ecb_block(&keys.hp, &sample);
        packet[0] ^= mask[0] & 0x0f;
        for i in 0..pn_len {
            packet[pn_offset + i] ^= mask[1 + i];
        }
        packet
    }
}
