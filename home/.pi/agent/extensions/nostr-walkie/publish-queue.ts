/**
 * In-memory outbox with per-relay circuit breaker.
 *
 * Port of opencrow's publish_queue.go minus SQLite persistence (this
 * extension is per-session, so crash-durability isn't worth the dep).
 *
 * Design:
 *   - One queue, drained by a single background loop.
 *   - Retry timing is owned by per-relay breakers, not per-item timers —
 *     one dead relay trips once instead of N items each backing off.
 *   - Breaker tracks *connectivity*: connect failures trip it; per-event
 *     rejects (content policy, auth) bump a per-(item,relay) counter and
 *     drop that relay from the item after rejectThreshold, without
 *     touching the breaker.
 *   - Two-tier TTL: 24h hard deadline if no relay accepted (data loss,
 *     log WARN), 10min best-effort for stragglers after first accept.
 */

import type { NostrEvent } from "nostr-tools";
import type { SimplePool } from "nostr-tools/pool";

// ─── tuning ──────────────────────────────────────────────────────────────

const PUBLISH_TIMEOUT_MS = 15_000;
const TTL_UNDELIVERED_MS = 24 * 60 * 60 * 1000;
const TTL_DELIVERED_MS = 10 * 60 * 1000;
/** Warn if nothing lands within this window so the user notices. */
const WARN_UNDELIVERED_MS = 30_000;

const BREAKER_THRESHOLD = 3;
const REJECT_THRESHOLD = 3;
const BREAKER_BASE_MS = 30_000;
const BREAKER_MAX_MS = 30 * 60 * 1000;

// ─── types ───────────────────────────────────────────────────────────────

interface Item {
  event: NostrEvent;
  /** relay URL → per-event reject count */
  pending: Map<string, number>;
  delivered: boolean;
  createdAt: number; // ms epoch
  warned: boolean;
  label: string;
}

type BreakerState = "closed" | "open" | "half-open";

interface Breaker {
  state: BreakerState;
  failures: number;
  cooldownMs: number;
  nextProbe: number; // ms epoch
}

// ─── queue ───────────────────────────────────────────────────────────────

export class PublishQueue {
  private items: Item[] = [];
  private breakers = new Map<string, Breaker>();
  private timer?: ReturnType<typeof setTimeout>;
  private draining = false;
  private stopped = false;

  constructor(
    private pool: SimplePool,
    private log: (level: "debug" | "info" | "warn", msg: string) => void,
  ) {}

  enqueue(event: NostrEvent, relays: string[], label = "event"): void {
    if (this.stopped || relays.length === 0) return;
    const pending = new Map<string, number>();
    for (const r of relays) pending.set(r, 0);
    this.items.push({
      event,
      pending,
      delivered: false,
      createdAt: Date.now(),
      warned: false,
      label,
    });
    this.wake();
  }

  stop(): void {
    this.stopped = true;
    if (this.timer) clearTimeout(this.timer);
  }

  // ─── drain loop ───────────────────────────────────────────────────────

  private wake(): void {
    if (this.draining || this.stopped) return;
    if (this.timer) clearTimeout(this.timer);
    this.timer = setTimeout(() => void this.drain(), 0);
  }

  private schedule(ms: number): void {
    if (this.stopped) return;
    if (this.timer) clearTimeout(this.timer);
    this.timer = setTimeout(() => void this.drain(), ms);
  }

  private async drain(): Promise<void> {
    if (this.draining || this.stopped) return;
    this.draining = true;
    try {
      this.evict();
      const work = this.collectWork();
      for (const [relay, items] of work) {
        if (this.stopped) break;
        await this.publishBatch(relay, items);
      }
      this.evict();
      this.gcBreakers();
      const next = this.nextWake();
      if (next > 0) this.schedule(next);
    } finally {
      this.draining = false;
    }
  }

  /** Group pending items by relay, consulting breakers. Half-open gets one probe. */
  private collectWork(): Map<string, Item[]> {
    const now = Date.now();
    const work = new Map<string, Item[]>();
    const probes = new Set<string>();
    for (const item of this.items) {
      for (const relay of item.pending.keys()) {
        const b = this.breaker(relay);
        const allow = this.allow(b, now);
        if (allow === "deny") continue;
        if (allow === "probe") {
          if (probes.has(relay)) continue;
          probes.add(relay);
        }
        const list = work.get(relay);
        if (list) list.push(item);
        else work.set(relay, [item]);
      }
    }
    return work;
  }

  private async publishBatch(relay: string, items: Item[]): Promise<void> {
    const b = this.breaker(relay);
    // SimplePool.publish returns one promise per relay; by passing a single
    // relay we get a single-element array. We treat connection errors as
    // breaker failures and content rejections as per-item rejects.
    for (const item of items) {
      if (this.stopped) return;
      let result: string;
      try {
        const [p] = this.pool.publish([relay], item.event);
        result = await withTimeout(p, PUBLISH_TIMEOUT_MS);
      } catch (e) {
        result = `connection failure: ${(e as Error).message}`;
      }
      if (isConnectFailure(result)) {
        this.onBreakerFailure(b, relay);
        return; // stop batch; relay is down
      }
      if (isReject(result)) {
        this.recordReject(item, relay, result);
        continue;
      }
      // accepted
      item.pending.delete(relay);
      if (!item.delivered) {
        item.delivered = true;
        this.log("debug", `nostr-walkie: ${item.label} accepted by ${relay}`);
      }
    }
    // Reached the relay → healthy regardless of per-event outcome.
    this.onBreakerSuccess(b, relay);
  }

  private recordReject(item: Item, relay: string, err: string): void {
    const n = (item.pending.get(relay) ?? 0) + 1;
    item.pending.set(relay, n);
    if (n >= REJECT_THRESHOLD) {
      item.pending.delete(relay);
      this.log(
        "warn",
        `nostr-walkie: ${relay} keeps rejecting ${item.label} (${err}), dropping from target set`,
      );
    }
  }

  private evict(): void {
    const now = Date.now();
    this.items = this.items.filter((item) => {
      const age = now - item.createdAt;
      if (!item.delivered && !item.warned && age > WARN_UNDELIVERED_MS) {
        item.warned = true;
        this.log(
          "warn",
          `nostr-walkie: ${item.label} not yet accepted by any relay after ${
            Math.round(age / 1000)
          }s`,
        );
      }
      if (item.pending.size === 0) {
        if (!item.delivered) {
          this.log(
            "warn",
            `nostr-walkie: DROPPING ${item.label} — rejected by all relays`,
          );
        }
        return false;
      }
      if (item.delivered && age > TTL_DELIVERED_MS) return false;
      if (!item.delivered && age > TTL_UNDELIVERED_MS) {
        this.log(
          "warn",
          `nostr-walkie: DROPPING undelivered ${item.label} past TTL (${
            Math.round(age / 1000)
          }s)`,
        );
        return false;
      }
      return true;
    });
  }

  private nextWake(): number {
    if (this.items.length === 0) return 0;
    const now = Date.now();
    const wanted = new Set<string>();
    for (const i of this.items) for (const r of i.pending.keys()) wanted.add(r);
    let earliest = Infinity;
    for (const r of wanted) {
      const b = this.breaker(r);
      if (b.state !== "open") return BREAKER_BASE_MS; // short nap
      if (b.nextProbe < earliest) earliest = b.nextProbe;
    }
    return earliest === Infinity ? 0 : Math.max(0, earliest - now);
  }

  // ─── circuit breaker ──────────────────────────────────────────────────

  private breaker(relay: string): Breaker {
    let b = this.breakers.get(relay);
    if (!b) {
      b = {
        state: "closed",
        failures: 0,
        cooldownMs: BREAKER_BASE_MS,
        nextProbe: 0,
      };
      this.breakers.set(relay, b);
    }
    return b;
  }

  private allow(b: Breaker, now: number): "all" | "probe" | "deny" {
    if (b.state === "closed") return "all";
    if (b.state === "open") {
      if (now >= b.nextProbe) {
        b.state = "half-open";
        return "probe";
      }
      return "deny";
    }
    return "deny"; // half-open: probe already scheduled this pass
  }

  private onBreakerSuccess(b: Breaker, relay: string): void {
    if (b.state !== "closed") {
      this.log("info", `nostr-walkie: relay ${relay} recovered`);
    }
    b.state = "closed";
    b.failures = 0;
    b.cooldownMs = BREAKER_BASE_MS;
  }

  private onBreakerFailure(b: Breaker, relay: string): void {
    b.failures++;
    const wasOpen = b.state === "open" || b.state === "half-open";
    if (b.state === "half-open" || b.failures >= BREAKER_THRESHOLD) {
      b.cooldownMs = wasOpen
        ? Math.min(b.cooldownMs * 2, BREAKER_MAX_MS)
        : BREAKER_BASE_MS;
      b.state = "open";
      b.nextProbe = Date.now() + b.cooldownMs;
      this.log(
        "warn",
        `nostr-walkie: relay ${relay} breaker open (cooldown ${
          b.cooldownMs / 1000
        }s)`,
      );
    }
  }

  private gcBreakers(): void {
    const wanted = new Set<string>();
    for (const i of this.items) for (const r of i.pending.keys()) wanted.add(r);
    const now = Date.now();
    for (const [r, b] of this.breakers) {
      if (wanted.has(r)) continue;
      if (b.state === "closed" || now - b.nextProbe > BREAKER_MAX_MS) {
        this.breakers.delete(r);
      }
    }
  }
}

// ─── helpers ─────────────────────────────────────────────────────────────

function withTimeout<T>(p: Promise<T>, ms: number): Promise<T> {
  return new Promise((resolve, reject) => {
    const t = setTimeout(
      () => reject(new Error(`timeout after ${ms}ms`)),
      ms,
    );
    p.then(
      (v) => {
        clearTimeout(t);
        resolve(v);
      },
      (e) => {
        clearTimeout(t);
        reject(e);
      },
    );
  });
}

/** SimplePool.publish resolves to a status string; these prefixes mean the
 *  relay was unreachable (connect/WS failure), not a content reject. */
function isConnectFailure(s: string): boolean {
  return (
    s.startsWith("connection failure:") ||
    s.startsWith("connection skipped") ||
    s.startsWith("timeout")
  );
}

/** Anything non-empty that isn't a connect failure and isn't an OK is a
 *  content-policy / auth reject from the relay. Empty string = accepted. */
function isReject(s: string): boolean {
  if (s === "" || s.startsWith("ok")) return false;
  return !isConnectFailure(s);
}
