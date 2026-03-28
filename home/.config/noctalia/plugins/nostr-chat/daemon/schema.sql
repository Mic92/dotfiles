CREATE TABLE IF NOT EXISTS messages (
  id      TEXT PRIMARY KEY,
  pubkey  TEXT NOT NULL,
  content TEXT NOT NULL,
  ts      INTEGER NOT NULL,
  dir     TEXT NOT NULL CHECK(dir IN ('in','out')),
  ack     TEXT NOT NULL DEFAULT '',
  read    INTEGER NOT NULL DEFAULT 0,
  image   TEXT NOT NULL DEFAULT '',
  reply_to TEXT NOT NULL DEFAULT '',
  -- Delivery state for outgoing messages: pending (in outbox) or sent
  -- (at least one relay accepted). Incoming rows default to 'sent' so
  -- the UI never shows a clock on the peer's bubbles.
  state   TEXT NOT NULL DEFAULT 'sent'
);
CREATE INDEX IF NOT EXISTS messages_ts ON messages(ts);

CREATE TABLE IF NOT EXISTS outbox (
  id        INTEGER PRIMARY KEY AUTOINCREMENT,
  -- Links back to messages.id so publish success/failure can update the
  -- bubble the user is looking at.
  rumor_id  TEXT NOT NULL,
  -- Serialised gift-wrap events. Persisting them means retries publish
  -- the same rumor id the local echo used, so the peer's ack lands on
  -- the right bubble. They're just signed JSON — the relay sees the
  -- exact same bytes either way.
  wrap_them TEXT NOT NULL,
  wrap_us   TEXT NOT NULL,
  tries     INTEGER NOT NULL DEFAULT 0,
  next_at   INTEGER NOT NULL
);

CREATE TABLE IF NOT EXISTS kv (
  k TEXT PRIMARY KEY,
  v TEXT NOT NULL
);
