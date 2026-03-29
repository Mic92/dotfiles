/**
 * Nostr NIP-17 DM send/receive + kind-15 file messages.
 *
 * Gift-wrap/seal/rumor dance via nostr-tools nip59 primitives; a
 * SimplePool handles relay connections. A single long-lived subscription
 * filters kind-1059 gift-wraps tagged to our pubkey, unwraps them, and
 * hands kind-14 text / kind-15 file rumors to the caller.
 *
 * Publishing goes through PublishQueue (circuit breaker + retry), not
 * raw SimplePool.publish — relays drop, rate-limit, and silently reject
 * all the time.
 */

import type { NostrEvent } from "nostr-tools";
import { unwrapEvent } from "nostr-tools/nip17";
import { decode, npubEncode } from "nostr-tools/nip19";
import { createRumor, createSeal, createWrap } from "nostr-tools/nip59";
import {
  SimplePool,
  type SubCloser,
  useWebSocketImplementation,
} from "nostr-tools/pool";
import { generateSecretKey, getPublicKey } from "nostr-tools/pure";
import WebSocket from "ws";
import {
  downloadAndDecrypt,
  fileRumorTags,
  parseFileRumor,
  uploadEncrypted,
} from "./blossom.ts";
import { PublishQueue } from "./publish-queue.ts";

// nostr-tools needs a WebSocket ctor in Node. Process-global side effect
// but harmless — we're the only consumer.
useWebSocketImplementation(WebSocket as unknown as typeof globalThis.WebSocket);

/** Parsed key material derived from the Config strings. */
export interface Keys {
  sk: Uint8Array;
  pk: string; // hex
  recipientPk: string; // hex
}

/** Unwrapped inbound payload we hand back to the extension. */
export type Inbound =
  | { kind: "text"; id: string; text: string; createdAt: number }
  | {
    kind: "file";
    id: string;
    path: string;
    mimeType: string;
    createdAt: number;
  };

export function parsePrivateKey(raw: string): Uint8Array {
  const s = raw.trim();
  if (s.startsWith("nsec1")) {
    const d = decode(s);
    if (d.type !== "nsec") throw new Error("expected nsec1…");
    return d.data as Uint8Array;
  }
  if (/^[0-9a-f]{64}$/i.test(s)) {
    return Uint8Array.from(Buffer.from(s, "hex"));
  }
  throw new Error("private key must be nsec1… or 64-char hex");
}

export function parsePubkey(raw: string): string {
  const s = raw.trim();
  if (s.startsWith("npub1")) {
    const d = decode(s);
    if (d.type !== "npub") throw new Error("expected npub1…");
    return d.data as string;
  }
  if (/^[0-9a-f]{64}$/i.test(s)) return s.toLowerCase();
  throw new Error("pubkey must be npub1… or 64-char hex");
}

export function deriveKeys(
  privateKey: string,
  recipientPubkey: string,
): Keys {
  const sk = parsePrivateKey(privateKey);
  return {
    sk,
    pk: getPublicKey(sk),
    recipientPk: parsePubkey(recipientPubkey),
  };
}

/** Generate a fresh keypair for the pi side. Returns hex strings. */
export function generateKeypair(): { sk: string; pk: string; npub: string } {
  const sk = generateSecretKey();
  const pk = getPublicKey(sk);
  return {
    sk: Buffer.from(sk).toString("hex"),
    pk,
    npub: npubEncode(pk),
  };
}

/** Cap on the dedup set so long sessions don't leak memory. */
const SEEN_CAP = 1000;

/**
 * Long-lived Nostr bridge: one SimplePool, one subscription, one outbox.
 * Call start() once, then sendDM()/sendFile()/sendReaction() as needed,
 * stop() on shutdown.
 */
export class NostrBridge {
  private pool: SimplePool;
  private queue: PublishQueue;
  private sub?: SubCloser;
  private seen = new Set<string>();
  /** Epoch seconds — ignore rumors older than this to avoid replaying
   *  yesterday's backlog into the agent on every restart. */
  private startedAt: number;
  private onMsg?: (m: Inbound) => void;

  constructor(
    private keys: Keys,
    private relays: string[],
    private blossomServers: string[],
    private log: (level: "debug" | "info" | "warn", msg: string) => void,
  ) {
    this.pool = new SimplePool();
    this.queue = new PublishQueue(this.pool, log);
    this.startedAt = Math.floor(Date.now() / 1000);
  }

  /**
   * Open the gift-wrap subscription. onMessage is called for each fresh
   * kind-14/15 rumor from the configured recipient.
   *
   * NIP-59 gift-wraps have created_at randomised up to 2 days into the
   * past, so the outer event timestamp is useless for ordering; we use
   * the inner rumor's created_at and compare against startedAt.
   *
   * SimplePool does not auto-reconnect dropped subscriptions, so we
   * re-subscribe from onclose with a small backoff.
   */
  start(onMessage: (m: Inbound) => void): void {
    this.onMsg = onMessage;
    this.subscribe();
  }

  private subscribe(): void {
    // Subscribe with a small lookback on the outer event so relays that
    // only serve recent history still match, but the rumor-timestamp
    // check in onevent is what actually gates delivery.
    const since = this.startedAt - 3 * 24 * 60 * 60;
    this.sub = this.pool.subscribe(
      this.relays,
      {
        kinds: [1059],
        "#p": [this.keys.pk],
        since: since > 0 ? since : 0,
      },
      {
        onevent: (ev) => this.onWrapped(ev),
        onclose: (reasons) => {
          // Called once *all* relays drop the sub. Re-open after a short
          // delay so a transient network blip doesn't kill inbound until
          // the next pi restart.
          if (!this.onMsg) return; // stopped
          this.log(
            "warn",
            `nostr-walkie: subscription closed (${
              reasons.join("; ")
            }), reconnecting in 5s`,
          );
          setTimeout(() => {
            if (this.onMsg) this.subscribe();
          }, 5000);
        },
      },
    );
  }

  private onWrapped(ev: NostrEvent): void {
    let rumor;
    try {
      rumor = unwrapEvent(ev, this.keys.sk);
    } catch {
      return; // not ours / malformed
    }
    if (rumor.pubkey !== this.keys.recipientPk) return;
    if (rumor.created_at < this.startedAt) return;
    if (this.seen.has(rumor.id)) return;
    if (this.seen.size >= SEEN_CAP) this.seen.clear();
    this.seen.add(rumor.id);

    if (rumor.kind === 14) {
      this.onMsg?.({
        kind: "text",
        id: rumor.id,
        text: rumor.content,
        createdAt: rumor.created_at,
      });
    } else if (rumor.kind === 15) {
      const meta = parseFileRumor(rumor);
      void downloadAndDecrypt(meta)
        .then(({ path, mimeType }) =>
          this.onMsg?.({
            kind: "file",
            id: rumor.id,
            path,
            mimeType,
            createdAt: rumor.created_at,
          })
        )
        .catch((e) =>
          this.log(
            "warn",
            `nostr-walkie: file download failed: ${(e as Error).message}`,
          )
        );
    }
  }

  /** Enqueue a NIP-17 text DM (kind 14). Also wraps a self-copy so our
   *  own Nostr client shows the outgoing message. */
  sendDM(text: string): void {
    this.wrapAndEnqueue(14, text, [], "DM");
  }

  /** Encrypt+upload a file, then enqueue a kind-15 rumor pointing at it. */
  async sendFile(filePath: string): Promise<void> {
    const up = await uploadEncrypted(
      filePath,
      this.blossomServers,
      this.keys.sk,
    );
    this.wrapAndEnqueue(15, up.url, fileRumorTags(up), "file");
  }

  /** NIP-25 reaction gift-wrapped to the recipient. Cheap and some
   *  clients render them (👀 on receive, ✅ on completion). */
  sendReaction(rumorId: string, emoji: string): void {
    const rumor = createRumor(
      {
        kind: 7,
        content: emoji,
        tags: [
          ["e", rumorId],
          ["p", this.keys.recipientPk],
          ["k", "14"],
        ],
      },
      this.keys.sk,
    );
    const seal = createSeal(rumor, this.keys.sk, this.keys.recipientPk);
    this.queue.enqueue(
      createWrap(seal, this.keys.recipientPk),
      this.relays,
      "reaction",
    );
  }

  private wrapAndEnqueue(
    kind: number,
    content: string,
    extraTags: string[][],
    label: string,
  ): void {
    const rumor = createRumor(
      {
        kind,
        content,
        tags: [["p", this.keys.recipientPk], ...extraTags],
      },
      this.keys.sk,
    );
    const seal = createSeal(rumor, this.keys.sk, this.keys.recipientPk);
    // Recipient copy is what matters; self-copy is best-effort convenience.
    this.queue.enqueue(
      createWrap(seal, this.keys.recipientPk),
      this.relays,
      `${label} toThem`,
    );
    this.queue.enqueue(
      createWrap(seal, this.keys.pk),
      this.relays,
      `${label} toUs`,
    );
  }

  stop(): void {
    this.onMsg = undefined;
    this.queue.stop();
    try {
      this.sub?.close();
    } catch {
      /* noop */
    }
    try {
      this.pool.destroy();
    } catch {
      /* noop */
    }
  }

  /** Our pubkey as npub for display. */
  npub(): string {
    return npubEncode(this.keys.pk);
  }
}

// re-export for the setup command to show the user
export { npubEncode };
