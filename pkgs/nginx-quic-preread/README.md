# ngx_stream_quic_preread

An nginx `stream {}` module, written in Rust, that does **QUIC preread**: it
peeks at the first datagram of a UDP connection, and if it is a QUIC Initial
packet it extracts the TLS **SNI** and **ALPN** from the ClientHello and exposes
them as run-time variables. This is the QUIC/UDP analogue of nginx's built-in
[`ssl_preread`](https://nginx.org/en/docs/stream/ngx_stream_ssl_preread_module.html),
which only works for TLS over TCP.

It lets you route QUIC / HTTP-3 traffic by server name **without terminating
TLS** — the encrypted datagrams are forwarded untouched to the chosen backend.

## How it can work at all

A QUIC Initial packet is encrypted and authenticated, but — unlike the rest of
the connection — with keys derived deterministically from the client's chosen
**Destination Connection ID** using a per-version salt (RFC 9001 §5.2). Any
on-path element can therefore decrypt the Initial packet **without the server's
private key**, which is exactly what makes SNI-based routing of QUIC possible.

The pipeline (all in [`src/quic.rs`](src/quic.rs)):

1. Parse the QUIC long header; recover the version and Destination Connection ID.
2. Derive the client Initial `key` / `iv` / `hp` (HKDF, RFC 9001 §5.2).
3. Remove header protection to learn the packet-number length (RFC 9001 §5.4).
4. AEAD-decrypt the payload with AES-128-GCM (RFC 9001 §5.3).
5. Reassemble the CRYPTO frames into the TLS handshake stream (RFC 9000 §19.6).
6. Parse the ClientHello and read `server_name` (RFC 6066) and ALPN (RFC 7301).

QUIC v1 (RFC 9000/9001) and QUIC v2 (RFC 9369) are both supported.

## Variables

| Variable                       | Meaning                                   |
| ------------------------------ | ----------------------------------------- |
| `$quic_preread_server_name`    | SNI host name from the ClientHello        |
| `$quic_preread_alpn_protocols` | comma-separated ALPN list (e.g. `h3,h3-29`) |

## Directive

```
Syntax:  quic_preread on | off;
Default: quic_preread off;
Context: stream, server
```

## Example

```nginx
stream {
    map $quic_preread_server_name $quic_backend {
        hostnames;
        app.example.org  127.0.0.1:8443;
        default          127.0.0.1:9443;
    }

    server {
        listen 443 udp reuseport;
        quic_preread on;
        proxy_pass $quic_backend;
    }
}
```

## Design

The Rust crate is split so that the risky, spec-heavy part is testable without
nginx:

- `src/quic.rs` — pure Rust, no nginx dependency. The QUIC/TLS logic and its
  tests. Build/test it standalone with `cargo test`.
- `src/stream_module.rs` — thin, unsafe FFI glue built on
  [`ngx`](https://crates.io/crates/ngx) / `nginx-sys`, modeled directly on
  nginx's own `ngx_stream_ssl_preread_module.c`. Compiled only with the `nginx`
  feature (off by default), which needs an nginx source tree at build time.

The module is linked **statically** into nginx via `--add-module`; the nginx
buildsystem invokes cargo through `config` / `config.make` / `auto/rust`
(vendored from ngx-rust). There is no dynamic `.so` and no `--with-compat`.

### Tests

`src/quic.rs` is validated against the authoritative specification vectors:

- **RFC 9001 Appendix A** — the published `key` / `iv` / `hp` for a known
  Destination Connection ID (validates the salt, HKDF-Extract, HKDF-Expand-Label
  and every label string).
- **RFC 9369 Appendix A** — the same for QUIC v2.
- Direct ClientHello / CRYPTO-frame parser tests, including a ClientHello split
  across two out-of-order CRYPTO frames.
- A full **encrypt → preread round-trip** for both QUIC versions, exercising the
  header-protection offsets, the AEAD nonce/AAD construction and the on-wire
  packet framing.

```console
$ cargo test
test result: ok. 7 passed; 0 failed
```

There is also a NixOS VM **integration test** (`test.nix`) that builds nginx with
the module, fires a real QUIC Initial packet (generated independently by
`aioquic`) at a UDP stream server, and asserts it is proxied to the
SNI-matched backend. It is wired into the flake checks as
`package-nginx-quic-preread-test-sni-routing`.

## Building

The module is compiled statically into nginx. With Nix (from the repo root),
`nginx-quic-preread` is nginx with the module linked in:

```console
$ nix build .#nginx-quic-preread
$ nix build .#checks.x86_64-linux.package-nginx-quic-preread-test-sni-routing
```

## NixOS

`nixosModules/nginx-quic-preread.nix` wires it up via
`services.nginx.additionalModules` (which recompiles nginx with the module):

```nix
{
  imports = [ ./nixosModules/nginx-quic-preread.nix ];
  services.nginx.quicPreread.enable = true;
  services.nginx.streamConfig = ''
    map $quic_preread_server_name $quic_backend { ... }
    server { listen 443 udp reuseport; quic_preread on; proxy_pass $quic_backend; }
  '';
}
```

## Limitations

- **First datagram only.** The SNI lives at the front of the ClientHello, which
  in practice fits in the first Initial packet, so this is almost always enough.
  A ClientHello large enough to span multiple Initial datagrams (e.g. a very
  large post-quantum key share) may not be fully reassembled; if the SNI is not
  in the first datagram the variable is empty and traffic takes the `default`
  route.
- **Retry / version negotiation** packets are not Initial packets and are
  ignored (the variables are empty), as are non-QUIC datagrams.
- Only the well-known Initial salts of QUIC v1 and v2 are known; experimental
  draft versions are reported as unsupported and left unrouted-by-name.

## References

- RFC 9000 — QUIC transport
- RFC 9001 — Using TLS to Secure QUIC (key derivation, packet protection)
- RFC 9369 — QUIC Version 2
- RFC 8446 — TLS 1.3 (ClientHello, HKDF-Expand-Label)
- RFC 6066 — TLS SNI · RFC 7301 — TLS ALPN
