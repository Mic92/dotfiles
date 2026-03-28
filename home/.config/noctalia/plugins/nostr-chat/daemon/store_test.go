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

	m := Message{ID: "abc", PubKey: "pk", Content: "hi", TS: 100, Dir: DirIn}
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

	ok, err := s.InsertMessage(ctx, Message{ID: "rumor1", PubKey: "me", Content: "hello", TS: 200, Dir: DirOut})
	if err != nil || !ok {
		t.Fatalf("upsert over stub: ok=%v err=%v", ok, err)
	}
	msgs, _ = s.Recent(ctx, 10)
	if len(msgs) != 1 || msgs[0].Content != "hello" || msgs[0].Ack != "👍" {
		t.Fatalf("want content+ack merged, got %+v", msgs)
	}
	// A second real insert must now dedup.
	ok, _ = s.InsertMessage(ctx, Message{ID: "rumor1", PubKey: "me", Content: "hello", TS: 200, Dir: DirOut})
	if ok {
		t.Fatal("dup after upsert should be false")
	}
}

func TestReplyToPersists(t *testing.T) {
	s, err := OpenStore(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()
	ctx := context.Background()

	// Message + its threaded reply — both round-trip through Recent().
	s.InsertMessage(ctx, Message{ID: "parent", Content: "q", TS: 100, Dir: DirIn})
	s.InsertMessage(ctx, Message{ID: "child", Content: "a", TS: 200, Dir: DirOut, ReplyTo: "parent"})

	msgs, _ := s.Recent(ctx, 10)
	if len(msgs) != 2 || msgs[1].ReplyTo != "parent" {
		t.Fatalf("replyTo lost: %+v", msgs)
	}
}

// Delivery state flows pending → sent and survives a replay.
func TestDeliveryState(t *testing.T) {
	s, err := OpenStore(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()
	ctx := context.Background()

	s.InsertMessage(ctx, Message{ID: "m1", Content: "hi", TS: 100, Dir: DirOut, State: StatePending})
	msgs, _ := s.Recent(ctx, 10)
	if len(msgs) != 1 || msgs[0].State != StatePending {
		t.Fatalf("pending not persisted: %+v", msgs)
	}

	if err := s.SetState(ctx, "m1", StateSent); err != nil {
		t.Fatal(err)
	}
	msgs, _ = s.Recent(ctx, 10)
	if msgs[0].State != StateSent {
		t.Fatalf("SetState didn't stick: %+v", msgs)
	}

	// Incoming messages default to sent — the UI never shows a clock on
	// the peer's bubbles.
	s.InsertMessage(ctx, Message{ID: "m2", Content: "yo", TS: 200, Dir: DirIn})
	msgs, _ = s.Recent(ctx, 10)
	if msgs[1].State != StateSent {
		t.Fatalf("incoming should default sent: %+v", msgs[1])
	}
}

func TestOutboxRoundTrip(t *testing.T) {
	s, err := OpenStore(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()
	ctx := context.Background()

	id, err := s.Enqueue(ctx, "rumor1", `{"them":1}`, `{"us":1}`)
	if err != nil {
		t.Fatal(err)
	}
	items, _ := s.PendingOutbox(ctx, 1)
	if len(items) != 1 || items[0].RumorID != "rumor1" || items[0].WrapThem != `{"them":1}` {
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

func TestOutboxCancel(t *testing.T) {
	s, err := OpenStore(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	defer s.Close()
	ctx := context.Background()

	s.InsertMessage(ctx, Message{ID: "r1", Content: "stuck", TS: 100, Dir: DirOut, State: StatePending})
	s.Enqueue(ctx, "r1", "{}", "{}")

	if err := s.OutboxCancel(ctx, "r1"); err != nil {
		t.Fatal(err)
	}
	if items, _ := s.PendingOutbox(ctx, 1); len(items) != 0 {
		t.Fatalf("outbox not cleared: %+v", items)
	}
	if msgs, _ := s.Recent(ctx, 10); len(msgs) != 0 {
		t.Fatalf("pending echo not deleted: %+v", msgs)
	}

	// A sent message must survive cancel — guard against the user
	// racing a tap against the publish completing.
	s.InsertMessage(ctx, Message{ID: "r2", Content: "ok", TS: 200, Dir: DirOut, State: StateSent})
	s.OutboxCancel(ctx, "r2")
	if msgs, _ := s.Recent(ctx, 10); len(msgs) != 1 {
		t.Fatalf("sent message wrongly deleted: %+v", msgs)
	}
}
