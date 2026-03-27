// Media support: NIP-17 kind-15 file messages over Blossom.
//
// Outgoing: encrypt with a fresh AES-256-GCM key, upload ciphertext to
// Blossom, send a kind-15 rumor carrying the URL + decryption params as
// tags. Incoming: download, decrypt in place, hand the local path to
// the shell so QML can render an <Image>.
//
// Lifted from nitrous (github.com/…/nitrous) with the bubbletea glue
// stripped — we run uploads/downloads in plain goroutines and push
// results straight to IPC.
package main

import (
	"bytes"
	"context"
	"crypto/aes"
	"crypto/cipher"
	"crypto/rand"
	"crypto/sha256"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"mime"
	"net/http"
	"net/url"
	"os"
	"path"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"time"

	"fiatjaf.com/nostr"
)

// KindFileMessage is the NIP-17 kind for file messages.
const KindFileMessage nostr.Kind = 15

// 50 MiB minus GCM tag — keeps ciphertext under the download cap.
const maxUploadSize = 50<<20 - 16
const maxDownloadSize = 50 << 20

// ── crypto ──────────────────────────────────────────────────────────

type encryptedFile struct {
	Ciphertext []byte
	SHA256Hex  string // of ciphertext — Blossom auth + URL
	KeyHex     string
	NonceHex   string
	OxHex      string // sha256 of plaintext — integrity check on decrypt
	Mime       string
}

func encryptFile(srcPath string) (*encryptedFile, error) {
	data, err := os.ReadFile(srcPath)
	if err != nil {
		return nil, fmt.Errorf("read %s: %w", srcPath, err)
	}
	if int64(len(data)) > maxUploadSize {
		return nil, fmt.Errorf("file too large (%d bytes, max %d)", len(data), maxUploadSize)
	}
	mimeType := detectContentType(srcPath, data)

	key := make([]byte, 32)
	if _, err := rand.Read(key); err != nil {
		return nil, err
	}
	nonce := make([]byte, 12)
	if _, err := rand.Read(nonce); err != nil {
		return nil, err
	}
	block, err := aes.NewCipher(key)
	if err != nil {
		return nil, err
	}
	gcm, err := cipher.NewGCM(block)
	if err != nil {
		return nil, err
	}
	ox := sha256.Sum256(data)
	ct := gcm.Seal(nil, nonce, data, nil)
	ctHash := sha256.Sum256(ct)

	return &encryptedFile{
		Ciphertext: ct,
		SHA256Hex:  hex.EncodeToString(ctHash[:]),
		KeyHex:     hex.EncodeToString(key),
		NonceHex:   hex.EncodeToString(nonce),
		OxHex:      hex.EncodeToString(ox[:]),
		Mime:       mimeType,
	}, nil
}

func decryptFileInPlace(filePath string, tags nostr.Tags) error {
	if algo := tagValue(tags, "encryption-algorithm"); algo != "aes-gcm" {
		return fmt.Errorf("unsupported encryption algorithm %q", algo)
	}
	keyHex := tagValue(tags, "decryption-key")
	nonceHex := tagValue(tags, "decryption-nonce")
	if keyHex == "" || nonceHex == "" {
		return errors.New("missing decryption-key or decryption-nonce")
	}
	key, err := hex.DecodeString(keyHex)
	if err != nil {
		return fmt.Errorf("decode key: %w", err)
	}
	if len(key) != 16 && len(key) != 32 {
		return fmt.Errorf("key must be 16 or 32 bytes, got %d", len(key))
	}
	nonce, err := hex.DecodeString(nonceHex)
	if err != nil {
		return fmt.Errorf("decode nonce: %w", err)
	}

	ct, err := os.ReadFile(filePath)
	if err != nil {
		return err
	}
	block, err := aes.NewCipher(key)
	if err != nil {
		return err
	}
	// NewGCMWithNonceSize: some clients send non-standard nonce lengths.
	gcm, err := cipher.NewGCMWithNonceSize(block, len(nonce))
	if err != nil {
		return err
	}
	pt, err := gcm.Open(nil, nonce, ct, nil)
	if err != nil {
		return fmt.Errorf("aes-gcm open: %w", err)
	}
	if ox := tagValue(tags, "ox"); ox != "" {
		h := sha256.Sum256(pt)
		if hex.EncodeToString(h[:]) != ox {
			return errors.New("sha256 mismatch after decryption")
		}
	}
	return os.WriteFile(filePath, pt, 0o600)
}

// ── download ────────────────────────────────────────────────────────

var unsafeFilenameChars = regexp.MustCompile(`[<>:"/\\|?*\x00-\x1f]`)

// downloadFile fetches a kind-15 URL into cacheDir, decrypts it if tags
// carry encryption params, and returns the local path. Cached by URL
// hash so replays don't re-download.
func downloadFile(ctx context.Context, rawURL, cacheDir string, tags nostr.Tags) (string, error) {
	ext := ""
	if u, err := url.Parse(rawURL); err == nil {
		ext = filepath.Ext(path.Base(u.Path))
	}
	// Prefer extension from the declared mime — the ciphertext URL has
	// no extension, but the tag knows it's image/png.
	if mt := tagValue(tags, "file-type"); mt != "" {
		if exts, _ := mime.ExtensionsByType(mt); len(exts) > 0 {
			ext = exts[0]
		}
	}
	ext = unsafeFilenameChars.ReplaceAllString(ext, "")

	h := sha256.Sum256([]byte(rawURL))
	dst := filepath.Join(cacheDir, hex.EncodeToString(h[:8])+ext)
	if _, err := os.Stat(dst); err == nil {
		return dst, nil // cache hit
	}
	if err := os.MkdirAll(cacheDir, 0o700); err != nil {
		return "", err
	}

	ctx, cancel := context.WithTimeout(ctx, 30*time.Second)
	defer cancel()
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, rawURL, nil)
	if err != nil {
		return "", err
	}
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return "", fmt.Errorf("GET %s: status %d", rawURL, resp.StatusCode)
	}
	if cl := resp.Header.Get("Content-Length"); cl != "" {
		if n, _ := strconv.ParseInt(cl, 10, 64); n > maxDownloadSize {
			return "", fmt.Errorf("download too large (%d bytes)", n)
		}
	}

	f, err := os.Create(dst)
	if err != nil {
		return "", err
	}
	n, err := io.Copy(f, io.LimitReader(resp.Body, maxDownloadSize+1))
	f.Close()
	if err != nil || n > maxDownloadSize {
		os.Remove(dst)
		if err == nil {
			err = fmt.Errorf("download exceeds %d bytes", maxDownloadSize)
		}
		return "", err
	}

	if tagValue(tags, "encryption-algorithm") != "" {
		if err := decryptFileInPlace(dst, tags); err != nil {
			os.Remove(dst)
			return "", fmt.Errorf("decrypt: %w", err)
		}
	}
	return dst, nil
}

// ── upload ──────────────────────────────────────────────────────────

// blossomUpload PUTs ciphertext to each server with a kind-24242 auth
// header. Returns the first successful URL; partial failure is fine.
func blossomUpload(ctx context.Context, servers []string, enc *encryptedFile, keys Keys) (string, error) {
	if len(servers) == 0 {
		return "", errors.New("no blossom servers configured")
	}
	auth := nostr.Event{
		Kind:      24242,
		CreatedAt: nostr.Now(),
		Tags: nostr.Tags{
			{"t", "upload"},
			{"x", enc.SHA256Hex},
			{"expiration", fmt.Sprintf("%d", time.Now().Add(5*time.Minute).Unix())},
		},
	}
	if err := auth.Sign(keys.SK); err != nil {
		return "", fmt.Errorf("sign auth: %w", err)
	}
	authJSON, _ := json.Marshal(auth)
	authHeader := "Nostr " + base64.StdEncoding.EncodeToString(authJSON)

	var firstURL string
	var errs []string
	for _, srv := range servers {
		u := strings.TrimRight(srv, "/") + "/upload"
		// bytes.NewReader — strings.NewReader(string(…)) would copy the
		// whole buffer, doubling memory for a 50 MB upload.
		req, err := http.NewRequestWithContext(ctx, http.MethodPut, u, bytes.NewReader(enc.Ciphertext))
		if err != nil {
			errs = append(errs, fmt.Sprintf("%s: %v", srv, err))
			continue
		}
		req.ContentLength = int64(len(enc.Ciphertext))
		req.Header.Set("Authorization", authHeader)
		req.Header.Set("Content-Type", enc.Mime)

		client := &http.Client{Timeout: 30 * time.Second}
		resp, err := client.Do(req)
		if err != nil {
			errs = append(errs, fmt.Sprintf("%s: %v", srv, err))
			continue
		}
		body, _ := io.ReadAll(resp.Body)
		resp.Body.Close()
		if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusCreated {
			errs = append(errs, fmt.Sprintf("%s: HTTP %d %s", srv, resp.StatusCode, body))
			continue
		}
		var r struct {
			URL string `json:"url"`
		}
		_ = json.Unmarshal(body, &r)
		if r.URL == "" {
			r.URL = strings.TrimRight(srv, "/") + "/" + enc.SHA256Hex
		}
		if firstURL == "" {
			firstURL = r.URL
		}
	}
	if firstURL == "" {
		return "", fmt.Errorf("all blossom servers failed: %s", strings.Join(errs, "; "))
	}
	return firstURL, nil
}

// ── helpers ─────────────────────────────────────────────────────────

func detectContentType(filePath string, data []byte) string {
	if ext := filepath.Ext(filePath); ext != "" {
		if ct := mime.TypeByExtension(ext); ct != "" {
			return ct
		}
	}
	sniff := data
	if len(sniff) > 512 {
		sniff = sniff[:512]
	}
	return http.DetectContentType(sniff)
}

func tagValue(tags nostr.Tags, key string) string {
	for _, t := range tags {
		if len(t) >= 2 && t[0] == key {
			return t[1]
		}
	}
	return ""
}


