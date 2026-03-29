/**
 * nostr-walkie config: load/persist from ~/.pi/nostr-walkie.json (global)
 * and <cwd>/.pi/nostr-walkie.json (project-local overrides global).
 */

import { execSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";

/**
 * Private-key source. A bare string in the JSON is upgraded to
 * `{ type: "literal", value: s }` on load for backward compat.
 */
export type PrivateKeySource =
  | { type: "literal"; value: string } // nsec1… or 64-char hex
  | { type: "command"; value: string }; // shell command, stdout = key

export interface Config {
  /** Our secret key — literal or fetched via command (rbw/pass/…). */
  privateKey: PrivateKeySource;
  /** Recipient/controller pubkey (hex or npub1…) — we only accept DMs from this key */
  recipientPubkey: string;
  /** Relay URLs for publish + subscribe */
  relays: string[];
  /** Blossom servers for encrypted file transfer (empty = files disabled). */
  blossomServers: string[];
  /** Master switch — disable without deleting config */
  enabled: boolean;
}

const FILE = "nostr-walkie.json";

function globalPath(): string {
  return join(homedir(), ".pi", FILE);
}

function localPath(cwd: string): string {
  return join(cwd, ".pi", FILE);
}

function readJson(path: string): Partial<Config> | undefined {
  if (!existsSync(path)) return undefined;
  try {
    return JSON.parse(readFileSync(path, "utf8"));
  } catch {
    return undefined;
  }
}

function normalizeKey(raw: unknown): PrivateKeySource | undefined {
  if (typeof raw === "string") return { type: "literal", value: raw };
  if (raw && typeof raw === "object") {
    const o = raw as Record<string, unknown>;
    if (
      (o.type === "literal" || o.type === "command") &&
      typeof o.value === "string"
    ) {
      return { type: o.type, value: o.value };
    }
  }
  return undefined;
}

/**
 * Resolve a PrivateKeySource to the actual key string. For "command",
 * spawns the shell command and returns its stdout (trimmed). Throws on
 * non-zero exit or empty output.
 */
export function resolvePrivateKey(src: PrivateKeySource): string {
  if (src.type === "literal") return src.value;
  const out = execSync(src.value, {
    encoding: "utf8",
    stdio: ["ignore", "pipe", "inherit"],
  }).trim();
  if (!out) {
    throw new Error(`key command produced no output: ${src.value}`);
  }
  return out;
}

/**
 * Load config. Project-local overrides global on a per-key basis.
 * Returns undefined if no config exists anywhere.
 */
export function loadConfig(cwd: string): Config | undefined {
  const g = readJson(globalPath());
  const l = readJson(localPath(cwd));
  if (!g && !l) return undefined;
  const merged = { ...g, ...l } as Record<string, unknown>;
  const privateKey = normalizeKey(merged.privateKey);
  if (!privateKey || typeof merged.recipientPubkey !== "string") {
    return undefined;
  }
  return {
    privateKey,
    recipientPubkey: merged.recipientPubkey,
    relays: Array.isArray(merged.relays) && merged.relays.length
      ? (merged.relays as string[])
      : ["wss://relay.damus.io", "wss://nos.lol", "wss://relay.primal.net"],
    blossomServers: Array.isArray(merged.blossomServers)
      ? (merged.blossomServers as string[])
      : [],
    enabled: merged.enabled !== false,
  };
}

/** Persist config to the global path (setup writes global by default). */
export function saveConfig(cfg: Config): string {
  const path = globalPath();
  mkdirSync(dirname(path), { recursive: true });
  writeFileSync(path, JSON.stringify(cfg, null, 2) + "\n", "utf8");
  return path;
}
