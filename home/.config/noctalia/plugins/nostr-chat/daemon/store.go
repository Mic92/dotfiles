package main

import (
	"context"
	"database/sql"
	_ "embed"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	_ "modernc.org/sqlite"
)

//go:embed schema.sql
var schema string

// Store owns the sqlite connection. A single daemon process writes, so
// we don't need WAL-level concurrency tricks — but WAL is still set so
// a stray `sqlite3` CLI read won't block the listen loop mid-INSERT.
type Store struct {
	db *sql.DB
}

// Message is the shape we hand to the shell. Direction is derived from
// pubkey at insert time so the QML side never has to compare pubkeys.
type Message struct {
	ID      string `json:"id"`
	PubKey  string `json:"pubkey"`
	Content string `json:"content"`
	TS      int64  `json:"ts"`
	Dir     string `json:"dir"` // "in" | "out"
	Ack     string `json:"ack"`
	Read    bool   `json:"read"`
	Image   string `json:"image,omitempty"`   // local path if kind-15 and downloaded
	ReplyTo string `json:"replyTo,omitempty"` // e-tag: rumor id this message responds to
}

func OpenStore(dir string) (*Store, error) {
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return nil, err
	}
	db, err := sql.Open("sqlite", filepath.Join(dir, "messages.db")+"?_pragma=journal_mode(WAL)&_pragma=busy_timeout(5000)")
	if err != nil {
		return nil, err
	}
	if _, err := db.Exec(schema); err != nil {
		db.Close()
		return nil, fmt.Errorf("schema: %w", err)
	}
	// Migration: image column. CREATE TABLE IF NOT EXISTS won't add
	// columns to an existing table, so patch it here and swallow the
	// "duplicate column" error on second run.
	for _, m := range []struct{ table, col string }{
		{"messages", "image"}, {"messages", "reply_to"}, {"outbox", "reply_to"},
	} {
		if _, err := db.Exec(fmt.Sprintf(`ALTER TABLE %s ADD COLUMN %s TEXT NOT NULL DEFAULT ''`, m.table, m.col)); err != nil &&
			!strings.Contains(err.Error(), "duplicate column") {
			db.Close()
			return nil, fmt.Errorf("migrate %s.%s: %w", m.table, m.col, err)
		}
	}
	return &Store{db: db}, nil
}

func (s *Store) Close() error { return s.db.Close() }

// InsertMessage returns false if the id already existed. Dedup lives
// here, not in memory — the rumor id is the primary key so replaying
// the 3-day lookback window after a restart is a no-op.
//
// An ack may have landed first and left a ts=0 stub; in that case we
// fill in the real content but keep the existing ack. The "WHERE ts=0"
// guard on the UPSERT ensures a genuine duplicate (same rumor, ts>0)
// still reports inserted=false.
func (s *Store) InsertMessage(ctx context.Context, m Message) (bool, error) {
	res, err := s.db.ExecContext(ctx,
		`INSERT INTO messages (id, pubkey, content, ts, dir, ack, read, image, reply_to)
		 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
		 ON CONFLICT(id) DO UPDATE SET
		   pubkey=excluded.pubkey, content=excluded.content,
		   ts=excluded.ts, dir=excluded.dir, read=excluded.read,
		   image=excluded.image, reply_to=excluded.reply_to
		 WHERE messages.ts = 0`,
		m.ID, m.PubKey, m.Content, m.TS, m.Dir, m.Ack, boolInt(m.Read), m.Image, m.ReplyTo)
	if err != nil {
		return false, err
	}
	n, _ := res.RowsAffected()
	return n > 0, nil
}

// SetAck records a kind-7 reaction against the rumor it targets.
// Reactions can race ahead of the self-copy (both travel through
// relays), so insert a stub row if the target isn't here yet — the real
// InsertMessage will OR IGNORE and keep our ack.
func (s *Store) SetAck(ctx context.Context, targetID, mark string) error {
	_, err := s.db.ExecContext(ctx,
		`INSERT INTO messages (id, pubkey, content, ts, dir, ack, read)
		 VALUES (?, '', '', 0, 'out', ?, 0)
		 ON CONFLICT(id) DO UPDATE SET ack = excluded.ack`,
		targetID, mark)
	return err
}

// SetImage records the local path of a downloaded kind-15 attachment.
// Called from the download goroutine after the message row already
// exists, so plain UPDATE.
func (s *Store) SetImage(ctx context.Context, id, path string) error {
	_, err := s.db.ExecContext(ctx,
		`UPDATE messages SET image = ? WHERE id = ?`, path, id)
	return err
}

// Recent returns the last n messages in chronological order, skipping
// ack-only stubs (ts=0 rows written by SetAck before the real message
// arrived).
func (s *Store) Recent(ctx context.Context, n int) ([]Message, error) {
	rows, err := s.db.QueryContext(ctx,
		`SELECT id, pubkey, content, ts, dir, ack, read, image, reply_to
		 FROM messages WHERE ts > 0 ORDER BY ts DESC LIMIT ?`, n)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var out []Message
	for rows.Next() {
		var m Message
		var read int
		if err := rows.Scan(&m.ID, &m.PubKey, &m.Content, &m.TS, &m.Dir, &m.Ack, &read, &m.Image, &m.ReplyTo); err != nil {
			return nil, err
		}
		m.Read = read != 0
		out = append(out, m)
	}
	// Reverse to chronological — cheaper than ORDER BY ASC + OFFSET math.
	for i, j := 0, len(out)-1; i < j; i, j = i+1, j-1 {
		out[i], out[j] = out[j], out[i]
	}
	return out, rows.Err()
}

func (s *Store) UnreadCount(ctx context.Context) (int, error) {
	var n int
	err := s.db.QueryRowContext(ctx,
		`SELECT COUNT(*) FROM messages WHERE read = 0 AND dir = 'in' AND ts > 0`).Scan(&n)
	return n, err
}

func (s *Store) MarkAllRead(ctx context.Context) error {
	_, err := s.db.ExecContext(ctx, `UPDATE messages SET read = 1 WHERE read = 0`)
	return err
}

// KV helpers — last_seen_ts and cached pubkey live here so a fresh
// subscribe doesn't have to re-derive or re-scan from epoch.

func (s *Store) GetInt(ctx context.Context, k string) (int64, error) {
	var v int64
	err := s.db.QueryRowContext(ctx, `SELECT v FROM kv WHERE k = ?`, k).Scan(&v)
	if errors.Is(err, sql.ErrNoRows) {
		return 0, nil
	}
	return v, err
}

func (s *Store) SetInt(ctx context.Context, k string, v int64) error {
	_, err := s.db.ExecContext(ctx,
		`INSERT INTO kv (k, v) VALUES (?, ?) ON CONFLICT(k) DO UPDATE SET v = excluded.v`, k, v)
	return err
}

// Outbox — sends are INSERTed first, published second, DELETEd on
// success. A crash between the first two steps re-sends, which is fine:
// NIP-17 has no idempotency anyway and a duplicate DM beats a lost one.

type OutboxItem struct {
	ID      int64
	Content string
	ReplyTo string
	Tries   int
}

func (s *Store) Enqueue(ctx context.Context, content, replyTo string) (int64, error) {
	res, err := s.db.ExecContext(ctx,
		`INSERT INTO outbox (content, reply_to, tries, next_at) VALUES (?, ?, 0, 0)`, content, replyTo)
	if err != nil {
		return 0, err
	}
	return res.LastInsertId()
}

func (s *Store) PendingOutbox(ctx context.Context, now int64) ([]OutboxItem, error) {
	rows, err := s.db.QueryContext(ctx,
		`SELECT id, content, reply_to, tries FROM outbox WHERE next_at <= ? ORDER BY id`, now)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var out []OutboxItem
	for rows.Next() {
		var it OutboxItem
		if err := rows.Scan(&it.ID, &it.Content, &it.ReplyTo, &it.Tries); err != nil {
			return nil, err
		}
		out = append(out, it)
	}
	return out, rows.Err()
}

func (s *Store) OutboxDone(ctx context.Context, id int64) error {
	_, err := s.db.ExecContext(ctx, `DELETE FROM outbox WHERE id = ?`, id)
	return err
}

func (s *Store) OutboxRetry(ctx context.Context, id int64, nextAt int64) error {
	_, err := s.db.ExecContext(ctx,
		`UPDATE outbox SET tries = tries + 1, next_at = ? WHERE id = ?`, nextAt, id)
	return err
}

func boolInt(b bool) int {
	if b {
		return 1
	}
	return 0
}
