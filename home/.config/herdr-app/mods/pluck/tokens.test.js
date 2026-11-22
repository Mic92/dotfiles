import assert from "node:assert/strict";
import test from "node:test";
import { hints, tokens } from "./tokens.js";

test("finds urls, paths, hashes and addresses, nearest line first", () => {
  const screen = [
    "see https://example.com/a?b=1, or (http://x.org/y).",
    "error in ./src/main.rs:12 and /nix/store/abc-foo/bin/x",
    "commit 2c88a56cb on 10.0.0.7",
    "hash sha256-Qz3YD5j5+wuvc3bF1Za99yrUD323aM35x4ZbimjEzlQ=",
  ].join("\n");
  assert.deepEqual(tokens(screen), [
    "sha256-Qz3YD5j5+wuvc3bF1Za99yrUD323aM35x4ZbimjEzlQ=",
    "10.0.0.7",
    "2c88a56cb",
    "/nix/store/abc-foo/bin/x",
    "./src/main.rs",
    "http://x.org/y",
    "https://example.com/a?b=1",
  ]);
});

test("repeated tokens get one hint", () => {
  assert.deepEqual(tokens("/a/b /a/b\n/a/b"), ["/a/b"]);
});

test("hints stay prefix-free", () => {
  assert.deepEqual(hints(3), ["a", "s", "d"]);
  const many = hints(40);
  assert.equal(new Set(many).size, 40);
  assert.ok(many.every((h) => h.length === 2));
});
