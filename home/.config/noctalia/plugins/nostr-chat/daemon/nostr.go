package main

import (
	"context"
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

// Publish sends both wraps to our relays. We skip nip17.GetDMRelays —
// the bot advertises the same relays we use, and the extra lookup adds
// latency to every send. Revisit if the peer uses different relays.
func (l *Listener) Publish(ctx context.Context, out Outgoing) error {
	ctx, cancel := context.WithTimeout(ctx, 15*time.Second)
	defer cancel()
	var ok int
	for _, ev := range []nostr.Event{out.toThem, out.toUs} {
		for res := range l.pool.PublishMany(ctx, l.relays, ev) {
			if res.Error == nil {
				ok++
			} else {
				slog.Debug("publish", "relay", res.RelayURL, "err", res.Error)
			}
		}
	}
	if ok == 0 {
		return fmt.Errorf("publish: no relay accepted")
	}
	return nil
}
