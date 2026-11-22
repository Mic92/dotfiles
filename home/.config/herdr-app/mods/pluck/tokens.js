// Earlier alternatives win, so a URL is not also matched as a path.
const PATTERNS = [
  /https?:\/\/[^\s"'<>`]+[^\s"'<>`.,;:)\]]/,
  /(?:~|\.{1,2})?(?:\/[\w.+@%=-]+){2,}\/?/,
  /sha256-[0-9a-zA-Z=/+]{44}/,
  /[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/,
  /\b\d{1,3}(?:\.\d{1,3}){3}\b/,
  // git and nix hashes
  /\b[0-9a-f]{7,40}\b/,
  /\b[0-9a-z]{52}\b/,
];
const TOKEN = new RegExp(PATTERNS.map((p) => p.source).join("|"), "g");
const KEYS = "asdfghjklqwertyuiopzxcvbnm";
const MAX = KEYS.length * KEYS.length;

// Closest to the cursor first: the bottom of the screen gets the short hints.
export function tokens(screen) {
  const seen = new Set();
  for (const line of screen.split("\n").reverse()) {
    for (const m of [...line.matchAll(TOKEN)].reverse()) seen.add(m[0]);
  }
  return [...seen].slice(0, MAX);
}

export function hints(n) {
  if (n <= KEYS.length) return [...KEYS].slice(0, n);
  const out = [];
  for (const a of KEYS) for (const b of KEYS) out.push(a + b);
  return out.slice(0, n);
}
