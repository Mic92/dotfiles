package main

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"

	"fiatjaf.com/nostr"
)

// Encrypt → write ciphertext → decrypt-in-place must round-trip to the
// original bytes and verify the ox hash.
func TestEncryptDecryptRoundTrip(t *testing.T) {
	dir := t.TempDir()
	src := filepath.Join(dir, "img.png")
	data := bytes.Repeat([]byte("pixel"), 1000)
	if err := os.WriteFile(src, data, 0o600); err != nil {
		t.Fatal(err)
	}

	enc, err := encryptFile(src)
	if err != nil {
		t.Fatal(err)
	}
	if enc.Mime != "image/png" {
		t.Fatalf("mime = %q, want image/png", enc.Mime)
	}
	if bytes.Equal(enc.Ciphertext, data) {
		t.Fatal("ciphertext equals plaintext")
	}

	// Simulate the download side: write ciphertext to disk, decrypt in
	// place using the tags a kind-15 rumor would carry.
	dst := filepath.Join(dir, "dl")
	if err := os.WriteFile(dst, enc.Ciphertext, 0o600); err != nil {
		t.Fatal(err)
	}
	tags := nostr.Tags{
		{"encryption-algorithm", "aes-gcm"},
		{"decryption-key", enc.KeyHex},
		{"decryption-nonce", enc.NonceHex},
		{"ox", enc.OxHex},
	}
	if err := decryptFileInPlace(dst, tags); err != nil {
		t.Fatal(err)
	}
	got, _ := os.ReadFile(dst)
	if !bytes.Equal(got, data) {
		t.Fatalf("round-trip mismatch: got %d bytes, want %d", len(got), len(data))
	}
}

func TestDecryptRejectsBadOx(t *testing.T) {
	dir := t.TempDir()
	src := filepath.Join(dir, "x")
	os.WriteFile(src, []byte("hello"), 0o600)
	enc, _ := encryptFile(src)

	dst := filepath.Join(dir, "dl")
	os.WriteFile(dst, enc.Ciphertext, 0o600)
	tags := nostr.Tags{
		{"encryption-algorithm", "aes-gcm"},
		{"decryption-key", enc.KeyHex},
		{"decryption-nonce", enc.NonceHex},
		{"ox", "deadbeef"}, // wrong
	}
	if err := decryptFileInPlace(dst, tags); err == nil {
		t.Fatal("expected ox mismatch error")
	}
}
