package main

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"log/slog"
	"strings"
	"time"

	"fiatjaf.com/nostr"
	"fiatjaf.com/nostr/keyer"
	"fiatjaf.com/nostr/nip19"
	"fiatjaf.com/nostr/nip59"
)

// nip59Margin: gift-wrap created_at is randomised up to 2 days into the
// past, so subscribing from last-seen alone would miss events. 3 days
// gives a comfortable buffer (same margin nitrous uses).
const nip59Margin = 3 * 24 * time.Hour

type Keys struct {
	SK nostr.SecretKey
	PK nostr.PubKey
}

func loadKeys(raw string) (Keys, error) {
	raw = strings.TrimSpace(raw)
	var sk nostr.SecretKey
	if strings.HasPrefix(raw, "nsec") {
		prefix, val, err := nip19.Decode(raw)
		if err != nil || prefix != "nsec" {
			return Keys{}, fmt.Errorf("decode nsec: %w", err)
		}
		sk = val.(nostr.SecretKey)
	} else {
		var err error
		if sk, err = nostr.SecretKeyFromHex(raw); err != nil {
			return Keys{}, fmt.Errorf("parse hex sk: %w", err)
		}
	}
	return Keys{SK: sk, PK: nostr.GetPublicKey(sk)}, nil
}

// Rumor is what we extract from an unwrapped gift — just enough to
// route kind-14 vs kind-7 without hauling the full event around.
type Rumor struct {
	ID      string
	Kind    nostr.Kind
	PubKey  string
	Content string
	TS      int64
	ETag    string     // first "e" tag: reaction target (kind-7) or reply parent (kind-14)
	Tags    nostr.Tags // kind-15 needs file-type / decryption-* tags
}

// Listener subscribes to kind-1059 gift wraps addressed to us and
// unwraps them. We don't use nip17.ListenForMessages because it
// silently drops anything that isn't kind-14, but peers may also wrap
// kind-7 reactions (read receipts) and kind-15 files the same way.
type Listener struct {
	pool   *nostr.Pool
	kr     nostr.Keyer
	keys   Keys
	relays []string
}

func NewListener(keys Keys, relays []string) *Listener {
	kr := keyer.NewPlainKeySigner(keys.SK)
	pool := nostr.NewPool(nostr.PoolOptions{
		// Relays that require NIP-42 auth will drop 1059 subs otherwise.
		AuthRequiredHandler: func(ctx context.Context, ev *nostr.Event) error {
			return kr.SignEvent(ctx, ev)
		},
	})
	return &Listener{pool: pool, kr: kr, keys: keys, relays: relays}
}

// Run blocks until ctx is done, emitting unwrapped rumors on ch.
// Pool.SubscribeMany handles per-relay reconnect internally; we only
// loop here to recover from the channel closing entirely (all relays
// dead at once). `since` is re-read from the store on each reconnect
// so a drop after an hour doesn't re-fetch the whole hour.
func (l *Listener) Run(ctx context.Context, since func() int64, ch chan<- Rumor) {
	backoff := time.Second
	for ctx.Err() == nil {
		start := time.Now()
		l.subscribeOnce(ctx, since(), ch)
		// A subscription that ran for a while was healthy — reset
		// backoff so a brief blip doesn't leave us at 1-minute delays
		// forever.
		if time.Since(start) > 30*time.Second {
			backoff = time.Second
		}
		slog.Warn("subscription closed, reconnecting", "backoff", backoff)
		select {
		case <-ctx.Done():
			return
		case <-time.After(backoff):
		}
		if backoff < time.Minute {
			backoff *= 2
		}
	}
}

func (l *Listener) subscribeOnce(ctx context.Context, since int64, ch chan<- Rumor) {
	adj := nostr.Timestamp(since) - nostr.Timestamp(nip59Margin.Seconds())
	if adj < 0 {
		adj = 0
	}
	slog.Info("subscribing", "relays", l.relays, "since", since, "adjusted", int64(adj))
	filter := nostr.Filter{
		Kinds: []nostr.Kind{nostr.KindGiftWrap},
		Tags:  nostr.TagMap{"p": {l.keys.PK.Hex()}},
		Since: adj,
	}
	for ev := range l.pool.SubscribeMany(ctx, l.relays, filter, nostr.SubscriptionOptions{}) {
		rumor, err := nip59.GiftUnwrap(ev.Event,
			func(pk nostr.PubKey, ct string) (string, error) {
				return l.kr.Decrypt(ctx, ct, pk)
			})
		if err != nil {
			// Not ours, or malformed — a relay serving someone else's
			// p-tag match would hit this. Quietly skip.
			continue
		}
		r := Rumor{
			ID:      rumor.ID.Hex(),
			Kind:    rumor.Kind,
			PubKey:  rumor.PubKey.Hex(),
			Content: rumor.Content,
			TS:      int64(rumor.CreatedAt),
			Tags:    rumor.Tags,
		}
		for _, t := range rumor.Tags {
			if len(t) >= 2 && t[0] == "e" {
				r.ETag = t[1]
				break
			}
		}
		select {
		case ch <- r:
		case <-ctx.Done():
			return
		}
	}
}

// Outgoing holds a built-but-unpublished DM. Split from the publish
// step so callers can echo locally (instant UI feedback) before the
// slow network round-trip.
type Outgoing struct {
	Rumor  Rumor
	toThem nostr.Event
	toUs   nostr.Event
}

// Wraps serialises both gift-wrap events so they can sit in the outbox
// and survive a daemon restart. They're just signed JSON — no secret
// material beyond what the relay will see anyway.
func (o Outgoing) Wraps() (them, us string) {
	t, _ := json.Marshal(o.toThem)
	u, _ := json.Marshal(o.toUs)
	return string(t), string(u)
}

// Prepare builds the kind-14 rumor and both gift wraps. Pure crypto,
// no network — microseconds. The returned Rumor has the final id, so
// the self-copy arriving later via the listen loop dedups cleanly.
// replyTo, if non-empty, is added as an e-tag so the peer can thread
// the response against a prior message.
func (l *Listener) Prepare(ctx context.Context, to, content, replyTo string) (Outgoing, error) {
	recipient, err := nostr.PubKeyFromHex(to)
	if err != nil {
		return Outgoing{}, fmt.Errorf("recipient pubkey: %w", err)
	}
	tags := nostr.Tags{{"p", to}}
	if replyTo != "" {
		tags = append(tags, nostr.Tag{"e", replyTo})
	}
	rumor := nostr.Event{
		Kind:      14,
		Content:   content,
		Tags:      tags,
		CreatedAt: nostr.Now(),
		PubKey:    l.keys.PK,
	}
	rumor.ID = rumor.GetID()

	wrap := func(pk nostr.PubKey) (nostr.Event, error) {
		return nip59.GiftWrap(rumor, pk,
			func(s string) (string, error) { return l.kr.Encrypt(ctx, s, pk) },
			func(e *nostr.Event) error { return l.kr.SignEvent(ctx, e) },
			nil)
	}
	toThem, err := wrap(recipient)
	if err != nil {
		return Outgoing{}, fmt.Errorf("wrap recipient: %w", err)
	}
	toUs, err := wrap(l.keys.PK)
	if err != nil {
		return Outgoing{}, fmt.Errorf("wrap self: %w", err)
	}
	return Outgoing{
		Rumor: Rumor{
			ID: rumor.ID.Hex(), Kind: rumor.Kind, PubKey: rumor.PubKey.Hex(),
			Content: rumor.Content, TS: int64(rumor.CreatedAt),
		},
		toThem: toThem, toUs: toUs,
	}, nil
}

// PrepareFile builds a kind-15 file rumor with encryption metadata.
// Same Prepare/Publish split as text: caller echoes locally first.
func (l *Listener) PrepareFile(ctx context.Context, to, url string, enc *encryptedFile) (Outgoing, error) {
	recipient, err := nostr.PubKeyFromHex(to)
	if err != nil {
		return Outgoing{}, fmt.Errorf("recipient pubkey: %w", err)
	}
	rumor := nostr.Event{
		Kind:    KindFileMessage,
		Content: url,
		Tags: nostr.Tags{
			{"p", to},
			{"file-type", enc.Mime},
			{"encryption-algorithm", "aes-gcm"},
			{"decryption-key", enc.KeyHex},
			{"decryption-nonce", enc.NonceHex},
			{"x", enc.SHA256Hex},
			{"ox", enc.OxHex},
		},
		CreatedAt: nostr.Now(),
		PubKey:    l.keys.PK,
	}
	rumor.ID = rumor.GetID()

	wrap := func(pk nostr.PubKey) (nostr.Event, error) {
		return nip59.GiftWrap(rumor, pk,
			func(s string) (string, error) { return l.kr.Encrypt(ctx, s, pk) },
			func(e *nostr.Event) error { return l.kr.SignEvent(ctx, e) },
			nil)
	}
	toThem, err := wrap(recipient)
	if err != nil {
		return Outgoing{}, fmt.Errorf("wrap recipient: %w", err)
	}
	toUs, err := wrap(l.keys.PK)
	if err != nil {
		return Outgoing{}, fmt.Errorf("wrap self: %w", err)
	}
	return Outgoing{
		Rumor: Rumor{
			ID: rumor.ID.Hex(), Kind: rumor.Kind, PubKey: rumor.PubKey.Hex(),
			Content: rumor.Content, TS: int64(rumor.CreatedAt), Tags: rumor.Tags,
		},
		toThem: toThem, toUs: toUs,
	}, nil
}

// PublishRaw sends serialised gift-wraps from the outbox to relays the
// subscription loop has already opened — no EnsureRelay, no 7s dial
// under the per-URL mutex shared with subscribe. A dead relay costs
// ~nothing, so the sequential outbox drain doesn't head-of-line block
// on timeouts. Reconnection is the listen loop's job; we just retry
// once it's back.
//
// This is the only publish path. Text and file sends alike enqueue
// their wraps and let publishLoop drain them, so both get retry/cancel
// and the same rumor id survives across attempts — the peer's ack lands
// on the bubble the user is staring at, not a phantom row.
func (l *Listener) PublishRaw(ctx context.Context, rumorID, themJSON, usJSON string) error {
	var them, us nostr.Event
	if err := json.Unmarshal([]byte(themJSON), &them); err != nil {
		return fmt.Errorf("decode wrap-them: %w", err)
	}
	if err := json.Unmarshal([]byte(usJSON), &us); err != nil {
		return fmt.Errorf("decode wrap-us: %w", err)
	}
	return l.publishConnected(ctx, rumorID, them, us)
}

func (l *Listener) publishConnected(ctx context.Context, rumorID string, evs ...nostr.Event) error {
	ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()
	var ok, fail, skip int
	for _, url := range l.relays {
		r, loaded := l.pool.Relays.Load(nostr.NormalizeURL(url))
		if !loaded || r == nil || !r.IsConnected() {
			skip++
			continue
		}
		for _, ev := range evs {
			if err := r.Publish(ctx, ev); err != nil {
				fail++
				slog.Debug("relay rejected", "relay", url, "err", err)
			} else {
				ok++
				slog.Debug("relay accepted", "relay", url)
			}
		}
	}
	slog.Debug("publish done", "rumor", rumorID[:8], "ok", ok, "fail", fail, "skip", skip)
	if ok == 0 {
		if skip == len(l.relays) {
			return ErrNoRelayConnected
		}
		return fmt.Errorf("publish: no relay accepted")
	}
	return nil
}

// ErrNoRelayConnected means we never reached a relay — the subscription
// hasn't (re)connected yet. Distinct from a rejection so publishLoop
// can defer without inflating the retry counter.
var ErrNoRelayConnected = errors.New("publish: no relay connected")
