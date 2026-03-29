/**
 * Blossom encrypted file transfer (BUD-01/02 + NIP-17 kind 15).
 *
 * Files are AES-256-GCM encrypted locally; only ciphertext touches the
 * Blossom server. Key+nonce travel inside the encrypted kind-15 rumor so
 * the server can't read the file and strangers can't decrypt the blob.
 *
 * Reference: opencrow/nostr/blossom.go, crypto.go, dm.go.
 */

import {
  createCipheriv,
  createDecipheriv,
  createHash,
  randomBytes,
} from "node:crypto";
import { mkdtempSync, readFileSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { basename, extname, join } from "node:path";
import type { NostrEvent } from "nostr-tools";
import { finalizeEvent } from "nostr-tools/pure";

export interface EncryptedUpload {
  url: string;
  mimeType: string;
  /** sha256 of ciphertext (NIP-17 "x" tag) */
  xHex: string;
  /** sha256 of plaintext (NIP-94 "ox" tag) */
  oxHex: string;
  keyHex: string;
  nonceHex: string;
  size: number;
}

/** Tags a kind-15 rumor must carry for the recipient to fetch+decrypt. */
export function fileRumorTags(u: EncryptedUpload): string[][] {
  return [
    ["file-type", u.mimeType],
    ["x", u.xHex],
    ["ox", u.oxHex],
    ["size", String(u.size)],
    ["encryption-algorithm", "aes-gcm"],
    ["decryption-key", u.keyHex],
    ["decryption-nonce", u.nonceHex],
  ];
}

export interface InboundFileMeta {
  url: string;
  mimeType: string;
  keyHex?: string;
  nonceHex?: string;
  algorithm?: string;
}

export function parseFileRumor(rumor: {
  content: string;
  tags: string[][];
}): InboundFileMeta {
  const tag = (k: string) =>
    rumor.tags.find((t) => t[0] === k)?.[1] ?? undefined;
  return {
    url: rumor.content,
    mimeType: tag("file-type") ?? tag("m") ?? "application/octet-stream",
    keyHex: tag("decryption-key") ?? tag("encryption-key"),
    nonceHex: tag("decryption-nonce") ?? tag("encryption-nonce"),
    algorithm: tag("encryption-algorithm"),
  };
}

// ─── upload ──────────────────────────────────────────────────────────────

export async function uploadEncrypted(
  filePath: string,
  servers: string[],
  sk: Uint8Array,
): Promise<EncryptedUpload> {
  if (servers.length === 0) {
    throw new Error("no blossom servers configured");
  }
  const plaintext = readFileSync(filePath);
  const mimeType = detectMime(filePath);
  const enc = encryptFile(plaintext);
  const xHex = sha256Hex(enc.ciphertext);
  const auth = blossomAuthHeader(xHex, enc.ciphertext.length, sk);

  let lastErr: unknown;
  for (const server of servers) {
    try {
      const url = await putBlob(server, enc.ciphertext, mimeType, auth, xHex);
      return {
        url,
        mimeType,
        xHex,
        oxHex: enc.oxHex,
        keyHex: enc.keyHex,
        nonceHex: enc.nonceHex,
        size: enc.ciphertext.length,
      };
    } catch (e) {
      lastErr = e;
    }
  }
  throw new Error(
    `all blossom servers failed: ${
      lastErr instanceof Error ? lastErr.message : String(lastErr)
    }`,
  );
}

async function putBlob(
  server: string,
  data: Buffer,
  mime: string,
  auth: string,
  xHex: string,
): Promise<string> {
  const url = server.replace(/\/$/, "") + "/upload";
  const res = await fetch(url, {
    method: "PUT",
    headers: { Authorization: auth, "Content-Type": mime },
    body: new Uint8Array(data),
  });
  const body = await res.text();
  if (res.status !== 200 && res.status !== 201) {
    throw new Error(`HTTP ${res.status}: ${body}`);
  }
  try {
    const j = JSON.parse(body) as { url?: string };
    if (j.url) return j.url;
  } catch {
    /* fallthrough */
  }
  return server.replace(/\/$/, "") + "/" + xHex;
}

function blossomAuthHeader(
  xHex: string,
  size: number,
  sk: Uint8Array,
): string {
  const exp = Math.floor(Date.now() / 1000) + 300;
  const evt: NostrEvent = finalizeEvent(
    {
      kind: 24242,
      created_at: Math.floor(Date.now() / 1000),
      content: "upload",
      tags: [
        ["t", "upload"],
        ["x", xHex],
        ["size", String(size)],
        ["expiration", String(exp)],
      ],
    },
    sk,
  );
  return "Nostr " + Buffer.from(JSON.stringify(evt)).toString("base64");
}

// ─── download ────────────────────────────────────────────────────────────

const MAX_DOWNLOAD = 50 << 20; // 50 MiB

/** Fetch a Blossom URL, decrypt if params present, write to tmpdir. */
export async function downloadAndDecrypt(
  meta: InboundFileMeta,
): Promise<{ path: string; mimeType: string }> {
  const res = await fetch(meta.url);
  if (!res.ok) throw new Error(`download HTTP ${res.status}`);
  const buf = Buffer.from(await res.arrayBuffer());
  if (buf.length > MAX_DOWNLOAD) {
    throw new Error(`download exceeds ${MAX_DOWNLOAD} bytes`);
  }
  let out = buf;
  if (meta.algorithm === "aes-gcm" && meta.keyHex && meta.nonceHex) {
    out = decryptFile(buf, meta.keyHex, meta.nonceHex);
  }
  const dir = mkdtempSync(join(tmpdir(), "nostr-walkie-"));
  const ext = mimeExt(meta.mimeType) || extFromUrl(meta.url) || "";
  const path = join(dir, "attachment" + ext);
  writeFileSync(path, out);
  return { path, mimeType: meta.mimeType };
}

// ─── crypto ──────────────────────────────────────────────────────────────

function encryptFile(plaintext: Buffer): {
  ciphertext: Buffer;
  keyHex: string;
  nonceHex: string;
  oxHex: string;
} {
  const key = randomBytes(32);
  const nonce = randomBytes(12);
  const cipher = createCipheriv("aes-256-gcm", key, nonce);
  const ct = Buffer.concat([cipher.update(plaintext), cipher.final()]);
  const tag = cipher.getAuthTag();
  return {
    ciphertext: Buffer.concat([ct, tag]), // GCM: append tag like Go's Seal
    keyHex: key.toString("hex"),
    nonceHex: nonce.toString("hex"),
    oxHex: sha256Hex(plaintext),
  };
}

function decryptFile(data: Buffer, keyHex: string, nonceHex: string): Buffer {
  const key = Buffer.from(keyHex, "hex");
  const nonce = Buffer.from(nonceHex, "hex");
  if (data.length < 16) throw new Error("ciphertext too short");
  const tag = data.subarray(data.length - 16);
  const ct = data.subarray(0, data.length - 16);
  const decipher = createDecipheriv("aes-256-gcm", key, nonce);
  decipher.setAuthTag(tag);
  return Buffer.concat([decipher.update(ct), decipher.final()]);
}

function sha256Hex(b: Buffer): string {
  return createHash("sha256").update(b).digest("hex");
}

// ─── mime ────────────────────────────────────────────────────────────────

const MIME: Record<string, string> = {
  ".png": "image/png",
  ".jpg": "image/jpeg",
  ".jpeg": "image/jpeg",
  ".gif": "image/gif",
  ".webp": "image/webp",
  ".pdf": "application/pdf",
  ".txt": "text/plain",
  ".json": "application/json",
  ".md": "text/markdown",
};

function detectMime(p: string): string {
  return MIME[extname(p).toLowerCase()] ?? "application/octet-stream";
}

function mimeExt(m: string): string {
  for (const [ext, mt] of Object.entries(MIME)) if (mt === m) return ext;
  return "";
}

function extFromUrl(u: string): string {
  try {
    const p = new URL(u).pathname;
    const e = extname(basename(p));
    return e.length <= 6 ? e : "";
  } catch {
    return "";
  }
}
