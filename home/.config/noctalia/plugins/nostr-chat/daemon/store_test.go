package main

import (
	"context"
	"testing"
)

func TestInsertDedup(t *testing.T) {
	s, err := OpenStore(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()
	ctx := context.Background()

	m := Message{ID: "abc", PubKey: "pk", Content: "hi", TS: 100, Dir: "in"}
	ok, err := s.InsertMessage(ctx, m)
	if err != nil || !ok {
		t.Fatalf("first insert: ok=%v err=%v", ok, err)
	}
	ok, err = s.InsertMessage(ctx, m)
	if err != nil || ok {
		t.Fatalf("dup insert should be (false,nil), got ok=%v err=%v", ok, err)
	}
}

// Reactions race the self-copy through relays — SetAck must be able to
// land first and survive the later InsertMessage.
func TestAckBeforeMessage(t *testing.T) {
	s, err := OpenStore(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()
	ctx := context.Background()

	if err := s.SetAck(ctx, "rumor1", "👍"); err != nil {
		t.Fatal(err)
	}
	// Stub row is ts=0, must not show up in Recent.
	msgs, _ := s.Recent(ctx, 10)
	if len(msgs) != 0 {
		t.Fatalf("stub leaked into Recent: %+v", msgs)
	}

	ok, err := s.InsertMessage(ctx, Message{ID: "rumor1", PubKey: "me", Content: "hello", TS: 200, Dir: "out"})
	if err != nil || !ok {
		t.Fatalf("upsert over stub: ok=%v err=%v", ok, err)
	}
	msgs, _ = s.Recent(ctx, 10)
	if len(msgs) != 1 || msgs[0].Content != "hello" || msgs[0].Ack != "👍" {
		t.Fatalf("want content+ack merged, got %+v", msgs)
	}
	// A second real insert must now dedup.
	ok, _ = s.InsertMessage(ctx, Message{ID: "rumor1", PubKey: "me", Content: "hello", TS: 200, Dir: "out"})
	if ok {
		t.Fatal("dup after upsert should be false")
	}
}

func TestOutboxRoundTrip(t *testing.T) {
	s, err := OpenStore(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()
	ctx := context.Background()

	id, err := s.Enqueue(ctx, "hello")
	if err != nil {
		t.Fatal(err)
	}
	items, _ := s.PendingOutbox(ctx, 1)
	if len(items) != 1 || items[0].Content != "hello" {
		t.Fatalf("pending = %+v", items)
	}
	if err := s.OutboxDone(ctx, id); err != nil {
		t.Fatal(err)
	}
	items, _ = s.PendingOutbox(ctx, 1)
	if len(items) != 0 {
		t.Fatalf("not drained: %+v", items)
	}
}
