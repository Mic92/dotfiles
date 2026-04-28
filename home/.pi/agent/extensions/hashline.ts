/**
 * Hashline — content-anchored line editing for pi.
 *
 * Compact synthesis of three implementations:
 *   - oh-my-pi (can1357):     BPE-bigram hash alphabet, ordinal trick, ±2 rebase
 *   - pi-hashline-edit (RimuruW): error codes, char-span conflict detection,
 *                                 post-edit anchor echo
 *   - pi-agent/hashline (pascal): pi ExtensionAPI integration shape
 *
 * Read format : `LINEbg|content`   (e.g. `12ab|  return x;`)
 * Anchor      : `LINEbg`           (line number + 2-letter bigram, no separator)
 *
 * The bigram alphabet is the 647 lowercase letter pairs that tokenize as a
 * single BPE token in cl100k/o200k/Claude vocabularies, so each anchor costs
 * ~2 tokens. Brace/blank lines map to the English ordinal suffix (st/nd/rd/th)
 * so `1st`, `42nd`, `100th` collapse to a single token.
 *
 * Zero runtime deps beyond @mariozechner/pi-coding-agent + @sinclair/typebox.
 * xxHash32 is inlined.
 */

import {
  createGrepTool,
  createReadTool,
  DEFAULT_MAX_BYTES,
  DEFAULT_MAX_LINES,
  type EditToolDetails,
  type ExtensionAPI,
  formatSize,
  truncateHead,
} from "@mariozechner/pi-coding-agent";
import { Type } from "@sinclair/typebox";
import { constants } from "node:fs";
import {
  access as fsAccess,
  readFile as fsReadFile,
  stat as fsStat,
  writeFile as fsWriteFile,
} from "node:fs/promises";
import * as os from "node:os";
import { isAbsolute, resolve as resolvePath } from "node:path";

// ═══════════════════════════════════════════════════════════════════════════
// xxHash32 (inlined, zero-dep)
// ═══════════════════════════════════════════════════════════════════════════

const XXH_P1 = 0x9e3779b1;
const XXH_P2 = 0x85ebca77;
const XXH_P3 = 0xc2b2ae3d;
const XXH_P4 = 0x27d4eb2f;
const XXH_P5 = 0x165667b1;

function rotl32(x: number, r: number): number {
  return ((x << r) | (x >>> (32 - r))) >>> 0;
}

function xxh32(input: string, seed = 0): number {
  // Hashing the UTF-8 encoding keeps results identical to Bun.hash.xxHash32
  // (oh-my-pi) so anchors are portable across implementations.
  const buf = Buffer.from(input, "utf8");
  const len = buf.length;
  let i = 0;
  let h: number;

  if (len >= 16) {
    let v1 = (seed + XXH_P1 + XXH_P2) >>> 0;
    let v2 = (seed + XXH_P2) >>> 0;
    let v3 = seed >>> 0;
    let v4 = (seed - XXH_P1) >>> 0;
    const limit = len - 16;
    while (i <= limit) {
      v1 = Math.imul(
        rotl32((v1 + Math.imul(buf.readUInt32LE(i), XXH_P2)) >>> 0, 13),
        XXH_P1,
      );
      v2 = Math.imul(
        rotl32((v2 + Math.imul(buf.readUInt32LE(i + 4), XXH_P2)) >>> 0, 13),
        XXH_P1,
      );
      v3 = Math.imul(
        rotl32((v3 + Math.imul(buf.readUInt32LE(i + 8), XXH_P2)) >>> 0, 13),
        XXH_P1,
      );
      v4 = Math.imul(
        rotl32((v4 + Math.imul(buf.readUInt32LE(i + 12), XXH_P2)) >>> 0, 13),
        XXH_P1,
      );
      i += 16;
    }
    h = (rotl32(v1, 1) + rotl32(v2, 7) + rotl32(v3, 12) + rotl32(v4, 18)) >>> 0;
  } else {
    h = (seed + XXH_P5) >>> 0;
  }

  h = (h + len) >>> 0;

  while (i + 4 <= len) {
    h = Math.imul(
      rotl32((h + Math.imul(buf.readUInt32LE(i), XXH_P3)) >>> 0, 17),
      XXH_P4,
    );
    i += 4;
  }
  while (i < len) {
    h = Math.imul(rotl32((h + Math.imul(buf[i], XXH_P5)) >>> 0, 11), XXH_P1);
    i++;
  }

  h ^= h >>> 15;
  h = Math.imul(h, XXH_P2);
  h ^= h >>> 13;
  h = Math.imul(h, XXH_P3);
  h ^= h >>> 16;
  return h >>> 0;
}

// ═══════════════════════════════════════════════════════════════════════════
// Bigram alphabet + line hash
// ═══════════════════════════════════════════════════════════════════════════

// 26×26 minus the 29 pairs no major BPE vocab merges into one token.
const EXCLUDED_BIGRAMS = new Set([
  "bq",
  "gk",
  "gq",
  "jv",
  "jz",
  "kq",
  "kz",
  "lq",
  "nq",
  "qf",
  "qg",
  "qj",
  "qk",
  "qv",
  "qz",
  "rj",
  "tq",
  "wq",
  "wz",
  "xg",
  "xj",
  "xk",
  "xq",
  "xv",
  "xw",
  "yq",
  "zj",
  "zq",
  "zv",
]);
const BIGRAMS: string[] = [];
for (let a = 97; a < 123; a++) {
  for (let b = 97; b < 123; b++) {
    const bg = String.fromCharCode(a, b);
    if (!EXCLUDED_BIGRAMS.has(bg)) BIGRAMS.push(bg);
  }
}
const BIGRAM_COUNT = BIGRAMS.length; // 647
const BIGRAM_SET = new Set(BIGRAMS);

const RE_SIGNIFICANT = /[\p{L}\p{N}]/u;
const RE_STRUCTURAL_STRIP = /[\s{}]/g;

function ordinalSuffix(n: number): string {
  const m100 = n % 100;
  if (m100 >= 11 && m100 <= 13) return "th";
  switch (n % 10) {
    case 1:
      return "st";
    case 2:
      return "nd";
    case 3:
      return "rd";
    default:
      return "th";
  }
}

function computeLineHash(idx: number, line: string): string {
  const norm = line.replace(/\r/g, "").trimEnd();
  if (norm.replace(RE_STRUCTURAL_STRIP, "").length === 0) {
    return ordinalSuffix(idx);
  }
  // Mixing the line number into non-alphanumeric lines reduces collisions
  // between repeated punctuation-only lines (e.g. `});` vs `});`).
  const seed = RE_SIGNIFICANT.test(norm) ? 0 : idx;
  return BIGRAMS[xxh32(norm, seed) % BIGRAM_COUNT];
}

function formatHashLine(n: number, line: string): string {
  return `${n}${computeLineHash(n, line)}|${line}`;
}

// ═══════════════════════════════════════════════════════════════════════════
// Anchor parsing + ±2 rebase
// ═══════════════════════════════════════════════════════════════════════════

type Anchor = { line: number; hash: string };
const REBASE_WINDOW = 2;

function parseAnchor(ref: string): Anchor {
  const m = ref.replace(/^\s*[>+-]*\s*/, "").match(/^(\d+)([a-z]{2})\b/);
  if (!m) {
    throw new Error(
      `[E_BAD_REF] Invalid anchor ${
        JSON.stringify(ref)
      }. Expected "LINE+2letters" copied from read output (e.g. "42nd").`,
    );
  }
  const line = Number.parseInt(m[1], 10);
  const hash = m[2];
  if (line < 1) {
    throw new Error(`[E_BAD_REF] Line number must be >= 1, got ${line}.`);
  }
  if (!BIGRAM_SET.has(hash) && !["st", "nd", "rd", "th"].includes(hash)) {
    throw new Error(
      `[E_BAD_REF] Anchor suffix "${hash}" is not a valid bigram.`,
    );
  }
  return { line, hash };
}

function tryRebase(anchor: Anchor, fileLines: string[]): number | null {
  const lo = Math.max(1, anchor.line - REBASE_WINDOW);
  const hi = Math.min(fileLines.length, anchor.line + REBASE_WINDOW);
  let found: number | null = null;
  for (let n = lo; n <= hi; n++) {
    if (n === anchor.line) continue;
    if (computeLineHash(n, fileLines[n - 1]) !== anchor.hash) continue;
    if (found !== null) return null; // ambiguous
    found = n;
  }
  return found;
}

// ═══════════════════════════════════════════════════════════════════════════
// Edit engine (char-span based, RimuruW-style conflict detection)
// ═══════════════════════════════════════════════════════════════════════════

type RawEdit = {
  op: "replace" | "append" | "prepend" | "replace_text";
  pos?: string;
  end?: string;
  lines?: string[] | string | null;
  oldText?: string;
  newText?: string;
};

type Span = {
  kind: "replace" | "insert";
  idx: number;
  label: string;
  start: number;
  end: number;
  text: string;
  /** Line-boundary index for inserts; used to detect two inserts at same point. */
  boundary?: number;
};

const HASHLINE_PREFIX_RE = /^\s*(?:>>>|>>)?\s*\d+[a-z]{2}\|/;

function normLines(v: string[] | string | null | undefined): string[] {
  if (v == null) return [];
  const lines = typeof v === "string"
    ? (v.endsWith("\n") ? v.slice(0, -1) : v).replace(/\r/g, "").split("\n")
    : v;
  for (const l of lines) {
    if (l.length && HASHLINE_PREFIX_RE.test(l)) {
      throw new Error(
        `[E_INVALID_PATCH] "lines" must contain literal file content, not "LINEbg|" display prefixes. Offending: ${
          JSON.stringify(l)
        }`,
      );
    }
  }
  return lines;
}

function formatMismatch(mismatches: Anchor[], fileLines: string[]): string {
  const show = new Set<number>();
  for (const m of mismatches) {
    for (
      let i = Math.max(1, m.line - 2);
      i <= Math.min(fileLines.length, m.line + 2);
      i++
    ) show.add(i);
  }
  const bad = new Set(mismatches.map((m) => m.line));
  const sorted = [...show].sort((a, b) => a - b);
  const out = [
    `[E_STALE_ANCHOR] ${mismatches.length} stale. Retry with >>> anchors:`,
    "",
  ];
  let prev = -1;
  for (const n of sorted) {
    if (prev !== -1 && n > prev + 1) out.push("    ...");
    prev = n;
    const mark = bad.has(n) ? ">>> " : "    ";
    out.push(mark + formatHashLine(n, fileLines[n - 1]));
  }
  return out.join("\n");
}

function applyEdits(content: string, raw: RawEdit[]): {
  content: string;
  warnings: string[];
  spans: Span[];
} {
  // Empty original: span offsets collapse to 0 and ordering becomes ambiguous,
  // so short-circuit with a direct prepend++append concatenation.
  if (content.length === 0) {
    const pre: string[] = [], app: string[] = [];
    for (const e of raw) {
      if ((e.op !== "append" && e.op !== "prepend") || e.pos) {
        throw new Error(
          `[E_RANGE_OOB] File is empty; only append/prepend without "pos" are valid.`,
        );
      }
      const lines = normLines(e.lines);
      if (lines.length === 0) {
        throw new Error(`[E_BAD_OP] ${e.op} requires non-empty "lines".`);
      }
      (e.op === "prepend" ? pre : app).push(lines.join("\n"));
    }
    return { content: [...pre, ...app].join("\n"), warnings: [], spans: [] };
  }

  const fileLines = content.split("\n");
  const lineStarts: number[] = [];
  {
    let off = 0;
    for (let i = 0; i < fileLines.length; i++) {
      lineStarts.push(off);
      off += fileLines[i].length + (i < fileLines.length - 1 ? 1 : 0);
    }
  }
  const lineEnd = (n: number) => lineStarts[n - 1] + fileLines[n - 1].length;

  const warnings: string[] = [];
  const mismatches: Anchor[] = [];

  /** Returns true on success (possibly after rebase), false if recorded as mismatch. */
  function validate(a: Anchor): boolean {
    if (a.line < 1 || a.line > fileLines.length) {
      throw new Error(
        `[E_RANGE_OOB] Line ${a.line} does not exist (file has ${fileLines.length} lines).`,
      );
    }
    const actual = computeLineHash(a.line, fileLines[a.line - 1]);
    if (actual === a.hash) return true;
    const re = tryRebase(a, fileLines);
    if (re !== null) {
      warnings.push(`rebased ${a.line}${a.hash}→${re}${a.hash}`);
      a.line = re;
      return true;
    }
    mismatches.push(a);
    return false;
  }

  const spans: Span[] = [];

  for (let idx = 0; idx < raw.length; idx++) {
    const e = raw[idx];
    const op = e.op;
    if (
      op !== "replace" && op !== "append" && op !== "prepend" &&
      op !== "replace_text"
    ) {
      throw new Error(
        `[E_BAD_OP] Unknown op "${op}". Expected replace|append|prepend|replace_text.`,
      );
    }

    if (op === "replace_text") {
      const oldText = (e.oldText ?? "").replace(/\r\n/g, "\n");
      const newText = (e.newText ?? "").replace(/\r\n/g, "\n");
      if (!oldText.length) {
        throw new Error(`[E_BAD_OP] replace_text requires non-empty oldText.`);
      }
      const first = content.indexOf(oldText);
      if (first === -1) {
        throw new Error(
          `[E_NO_MATCH] replace_text: oldText not found in ${"file"}.`,
        );
      }
      if (content.indexOf(oldText, first + 1) !== -1) {
        throw new Error(
          `[E_MULTI_MATCH] replace_text: oldText matches multiple times; use anchored replace.`,
        );
      }
      if (oldText === newText) continue;
      spans.push({
        kind: "replace",
        idx,
        label: `replace_text`,
        start: first,
        end: first + oldText.length,
        text: newText,
      });
      continue;
    }

    const lines = normLines(e.lines);

    if (op === "replace") {
      if (!e.pos) throw new Error(`[E_BAD_OP] replace requires "pos".`);
      // Missing `lines` is a model confusion (often mixed with oldText/newText),
      // not a delete request — only explicit null/[] deletes.
      if (e.lines === undefined) {
        throw new Error(
          `[E_BAD_OP] replace requires "lines" (use [] to delete).` +
            (e.oldText !== undefined ? ` Do not use oldText/newText with op:"replace".` : ""),
        );
      }
      const pos = parseAnchor(e.pos);
      const end = e.end ? parseAnchor(e.end) : pos;
      const origPos = pos.line, origEnd = end.line;
      const posOk = validate(pos);
      const endOk = end === pos ? posOk : validate(end);
      if (!posOk || !endOk) continue;
      // Reject rebase that changed the span size — replacing a different
      // number of lines than the model intended is a silent corruption risk.
      if (end.line - pos.line !== origEnd - origPos || pos.line > end.line) {
        mismatches.push({ line: origPos, hash: pos.hash }, {
          line: origEnd,
          hash: end.hash,
        });
        continue;
      }
      if (mismatches.length) continue;

      // Warn when the replacement re-emits the next surviving line — common
      // off-by-one when the model meant to extend `end` by one.
      const nextLine = fileLines[end.line];
      const lastNew = lines.at(-1)?.trim();
      if (
        nextLine !== undefined && lastNew && RE_SIGNIFICANT.test(lastNew) &&
        lastNew === nextLine.trim()
      ) {
        warnings.push(
          `dup? last new line == kept line ${end.line + 1}${
            computeLineHash(end.line + 1, nextLine)
          }`,
        );
      }

      if (lines.length === 0) {
        // Deletion: consume the trailing newline so no blank line is left behind.
        const isLast = end.line === fileLines.length;
        const start = isLast
          ? Math.max(0, lineStarts[pos.line - 1] - 1)
          : lineStarts[pos.line - 1];
        const stop = isLast ? lineEnd(end.line) : lineStarts[end.line];
        spans.push({
          kind: "replace",
          idx,
          label: `replace ${pos.line}-${end.line}`,
          start,
          end: stop,
          text: "",
        });
      } else {
        spans.push({
          kind: "replace",
          idx,
          label: `replace ${pos.line}-${end.line}`,
          start: lineStarts[pos.line - 1],
          end: lineEnd(end.line),
          text: lines.join("\n"),
        });
      }
      continue;
    }

    // append / prepend
    if (lines.length === 0) {
      throw new Error(`[E_BAD_OP] ${op} requires non-empty "lines".`);
    }
    const anchor = e.pos ? parseAnchor(e.pos) : undefined;
    if (anchor && !validate(anchor)) continue;
    if (mismatches.length) continue;

    const body = lines.join("\n");
    if (op === "append") {
      const at = anchor ? lineEnd(anchor.line) : content.length;
      const boundary = anchor ? anchor.line : fileLines.length;
      const text = at === content.length && content.endsWith("\n")
        ? body + "\n"
        : "\n" + body;
      spans.push({
        kind: "insert",
        idx,
        label: anchor ? `append after ${anchor.line}` : "append EOF",
        start: at,
        end: at,
        text,
        boundary,
      });
    } else {
      const at = anchor ? lineStarts[anchor.line - 1] : 0;
      const boundary = anchor ? anchor.line - 1 : 0;
      spans.push({
        kind: "insert",
        idx,
        label: anchor ? `prepend before ${anchor.line}` : "prepend BOF",
        start: at,
        end: at,
        text: body + "\n",
        boundary,
      });
    }
  }

  if (mismatches.length) throw new Error(formatMismatch(mismatches, fileLines));

  // Dedupe identical spans, then reject conflicting ones.
  const seen = new Set<string>();
  const uniq = spans.filter((s) => {
    const k = s.kind === "insert"
      ? `i:${s.boundary}:${s.text}`
      : `r:${s.start}:${s.end}:${s.text}`;
    if (seen.has(k)) return false;
    seen.add(k);
    return true;
  });

  for (let i = 0; i < uniq.length; i++) {
    for (let j = i + 1; j < uniq.length; j++) {
      const a = uniq[i], b = uniq[j];
      if (a.kind === "insert" && b.kind === "insert") {
        if (a.boundary === b.boundary) {
          conflict(a, b, "target the same insertion point");
        }
        continue;
      }
      if (a.kind === "replace" && b.kind === "replace") {
        if (a.start < b.end && b.start < a.end) conflict(a, b, "overlap");
        continue;
      }
      const r = a.kind === "replace" ? a : b;
      const ins = a.kind === "insert" ? a : b;
      if (ins.start > r.start && ins.start < r.end) {
        conflict(a, b, "insert inside replaced range");
      }
    }
  }

  // Apply highest-end first so earlier offsets stay valid. Tie-break: replace
  // before insert (so an insert at a replace's start lands before the new text),
  // then original index for determinism.
  uniq.sort((a, b) =>
    b.end - a.end ||
    (a.kind === b.kind ? 0 : a.kind === "replace" ? -1 : 1) ||
    a.idx - b.idx
  );
  let out = content;
  for (const s of uniq) out = out.slice(0, s.start) + s.text + out.slice(s.end);

  return { content: out, warnings, spans: uniq };

  function conflict(a: Span, b: Span, why: string): never {
    throw new Error(
      `[E_EDIT_CONFLICT] edit ${a.idx} (${a.label}) and edit ${b.idx} (${b.label}) ${why}. Merge them or split the request.`,
    );
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// Minimal diff (for UI) + changed-range (for anchor echo)
// ═══════════════════════════════════════════════════════════════════════════

function changedLineRange(
  a: string,
  b: string,
): { first: number; last: number } | null {
  if (a === b) return null;
  const al = a.split("\n"), bl = b.split("\n");
  let pre = 0;
  while (pre < al.length && pre < bl.length && al[pre] === bl[pre]) pre++;
  let sa = al.length - 1, sb = bl.length - 1;
  while (sa >= pre && sb >= pre && al[sa] === bl[sb]) {
    sa--;
    sb--;
  }
  // Clamp into the result's bounds so pure deletions don't report a line past EOF.
  const first = Math.min(pre + 1, Math.max(1, bl.length));
  const last = Math.max(first, Math.min(sb + 1, bl.length));
  return { first, last };
}

function simpleDiff(a: string, b: string): string {
  const r = changedLineRange(a, b);
  if (!r) return "";
  const al = a.split("\n"), bl = b.split("\n");
  // Recompute suffix to know how many old lines were replaced.
  let pre = r.first - 1;
  let sa = al.length - 1, sb = bl.length - 1;
  while (sa >= pre && sb >= pre && al[sa] === bl[sb]) {
    sa--;
    sb--;
  }
  const ctx = 3;
  const out: string[] = [];
  const lo = Math.max(0, pre - ctx);
  for (let i = lo; i < pre; i++) out.push(`  ${i + 1} ${bl[i]}`);
  for (let i = pre; i <= sa; i++) out.push(`- ${i + 1} ${al[i]}`);
  for (let i = pre; i <= sb; i++) out.push(`+ ${i + 1} ${bl[i]}`);
  const hi = Math.min(bl.length, sb + 1 + ctx);
  for (let i = sb + 1; i < hi; i++) out.push(`  ${i + 1} ${bl[i]}`);
  return out.join("\n");
}

// ═══════════════════════════════════════════════════════════════════════════
// Misc helpers
// ═══════════════════════════════════════════════════════════════════════════

function resolveToCwd(p: string, cwd: string): string {
  let s = p.startsWith("@") ? p.slice(1) : p;
  if (s === "~") return os.homedir();
  if (s.startsWith("~/")) return os.homedir() + s.slice(1);
  return isAbsolute(s) ? s : resolvePath(cwd, s);
}

function detectEOL(s: string): "\r\n" | "\n" {
  // Majority vote so a stray CRLF (e.g. inside a string literal) does not flip
  // the whole file's line endings on write.
  let crlf = 0, lf = 0;
  for (let i = 0; i < s.length; i++) {
    if (s[i] === "\n") s[i - 1] === "\r" ? crlf++ : lf++;
  }
  return crlf > lf ? "\r\n" : "\n";
}
function toLF(s: string): string {
  return s.replace(/\r\n/g, "\n").replace(/\r/g, "\n");
}
function stripBom(s: string): { bom: string; text: string } {
  return s.startsWith("\uFEFF")
    ? { bom: "\uFEFF", text: s.slice(1) }
    : { bom: "", text: s };
}

// ═══════════════════════════════════════════════════════════════════════════
// Extension entry
// ═══════════════════════════════════════════════════════════════════════════

// Prompts tuned for small models (Qwen 35B): one worked example + terse
// error→action map beats prose. Schema `description` fields carry per-arg docs.
const READ_DESC =
  `Read a file. Each text line is shown as ANCHOR|content (e.g. 12ab|  return x;). ` +
  `Use ANCHOR with the edit tool. Max ${DEFAULT_MAX_LINES} lines / ${
    formatSize(DEFAULT_MAX_BYTES)
  }.`;

const EDIT_DESC = `Edit a file by line anchor.

Example:
{"path":"a.ts","edits":[
 {"op":"replace","pos":"12ab","end":"14ir","lines":["  return y;"]},
 {"op":"append","pos":"20th","lines":["// new"]}
]}

ops: replace(pos,end?,lines) | append(pos?,lines) | prepend(pos?,lines)
- pos/end: anchor from read or grep (e.g. "12ab"). Omit pos on append/prepend = EOF/BOF.
- lines: raw content array. No "12ab|" prefixes. [] deletes.
- Prefer grep over read to get anchors when you know what to search for.

On error:
[E_STALE_ANCHOR] copy the >>> anchors shown and retry
[E_EDIT_CONFLICT] merge the two edits into one`;

// `replace_text` is accepted for compatibility but deliberately omitted from
// the description and op enum: small models reach for it first, hit
// [E_MULTI_MATCH] on repeated lines, and burn a turn. Anchored ops are primary.
const editItemSchema = Type.Object(
  {
    op: Type.String({ description: "replace | append | prepend" }),
    pos: Type.Optional(Type.String({ description: 'anchor e.g. "12ab"' })),
    end: Type.Optional(
      Type.String({ description: "range end anchor (replace only)" }),
    ),
    lines: Type.Optional(
      Type.Union([Type.Array(Type.String()), Type.String(), Type.Null()], {
        description: "new content lines; [] deletes",
      }),
    ),
    oldText: Type.Optional(Type.String()),
    newText: Type.Optional(Type.String()),
  },
  // Permit unadvertised `replace_text` for strong-model back-compat without
  // failing validation; applyEdits() enforces the real op set.
  { additionalProperties: true },
);

// `edits` also accepts a JSON string: small models (Qwen 35B observed)
// double-encode nested arrays. Validation lets it through; coerceEdits()
// normalizes inside execute() so the engine always sees RawEdit[].
const editSchema = Type.Object({
  path: Type.String({ description: "file path" }),
  edits: Type.Union([
    Type.Array(Type.Union([editItemSchema, Type.String()])),
    Type.String(),
  ]),
});

/**
 * Best-effort JSON parse for model-emitted argument strings.
 * Observed Qwen-35B failure mode: trailing `]`/`}` dropped on nested arrays.
 * Try once as-is, then once with missing closers appended.
 */
function tryParseJson(s: string): unknown {
  try {
    return JSON.parse(s);
  } catch { /* fallthrough */ }
  let open = 0, close = 0, str = false, esc = false;
  for (const c of s) {
    if (esc) { esc = false; continue; }
    if (c === "\\") { esc = true; continue; }
    if (c === '"') { str = !str; continue; }
    if (str) continue;
    if (c === "[" || c === "{") open++;
    else if (c === "]" || c === "}") close++;
  }
  if (open > close) {
    // Heuristic: append closers matching the first opener type seen.
    const fix = s + (s.trimStart().startsWith("[") ? "]" : "}").repeat(open - close);
    try {
      return JSON.parse(fix);
    } catch { /* fallthrough */ }
  }
  throw new Error(
    `[E_BAD_OP] "edits" is not valid JSON (check brackets): ${s.slice(0, 200)}`,
  );
}

function coerceEdits(input: unknown): RawEdit[] {
  let v: unknown = input;
  if (typeof v === "string") v = tryParseJson(v);
  if (!Array.isArray(v)) {
    throw new Error(`[E_BAD_OP] "edits" must be an array.`);
  }
  return v.map((e, i): RawEdit => {
    let item: unknown = e;
    if (typeof item === "string") item = tryParseJson(item);
    if (!item || typeof item !== "object") {
      throw new Error(`[E_BAD_OP] edits[${i}] must be an object.`);
    }
    const o = item as Record<string, unknown>;
    let lines = o.lines;
    if (typeof lines === "string" && /^\s*\[/.test(lines)) {
      try {
        const p = JSON.parse(lines);
        if (Array.isArray(p)) lines = p;
      } catch { /* keep raw string; normLines splits on \n */ }
    }
    return { ...o, lines } as RawEdit;
  });
}

const ANCHOR_ECHO_MAX = 14;

export default function (pi: ExtensionAPI): void {
  // ── read ───────────────────────────────────────────────────────────────
  pi.registerTool({
    name: "read",
    label: "Read",
    description: READ_DESC,
    promptSnippet: "read: Read file with line anchors for edit.",
    promptGuidelines: [
      "When editing a known symbol/function/string, use grep to get its anchor instead of reading the whole file.",
    ],
    parameters: Type.Object({
      path: Type.String({ description: "Path to the file to read" }),
      offset: Type.Optional(
        Type.Number({ description: "1-indexed start line" }),
      ),
      limit: Type.Optional(Type.Number({ description: "Max lines" })),
    }),

    async execute(toolCallId, params, signal, onUpdate, ctx) {
      const abs = resolveToCwd(params.path, ctx.cwd);
      try {
        await fsAccess(abs, constants.R_OK);
      } catch {
        return {
          content: [{
            type: "text",
            text: `File not found or not readable: ${params.path}`,
          }],
          isError: true,
          details: {},
        };
      }
      const st = await fsStat(abs);
      if (st.isDirectory()) {
        return {
          content: [{
            type: "text",
            text: `Path is a directory: ${params.path}`,
          }],
          isError: true,
          details: {},
        };
      }

      const ext = params.path.split(".").pop()?.toLowerCase() ?? "";
      if (["jpg", "jpeg", "png", "gif", "webp", "bmp", "svg"].includes(ext)) {
        return createReadTool(ctx.cwd).execute(
          toolCallId,
          params,
          signal,
          onUpdate,
        );
      }

      const raw = (await fsReadFile(abs)).toString("utf8");
      const text = toLF(stripBom(raw).text);
      const all = text.split("\n");
      const total = all.length;
      const start = params.offset ? Math.max(1, params.offset) : 1;
      const stop = params.limit
        ? Math.min(start - 1 + params.limit, total)
        : total;

      const formatted = all.slice(start - 1, stop).map((l, i) =>
        formatHashLine(start + i, l)
      ).join("\n");
      const tr = truncateHead(formatted, {
        maxLines: DEFAULT_MAX_LINES,
        maxBytes: DEFAULT_MAX_BYTES,
      });
      let out = tr.content;
      if (tr.truncated) {
        out += `\n\n[Truncated: ${tr.outputLines}/${total} lines. Use offset=${
          start + tr.outputLines
        } to continue.]`;
      } else if (stop < total) {
        out += `\n\n[Lines ${start}-${stop} of ${total}. Use offset=${
          stop + 1
        } to continue.]`;
      }
      return { content: [{ type: "text", text: out }], details: {} };
    },
  });

  // ── edit ───────────────────────────────────────────────────────────────
  pi.registerTool({
    name: "edit",
    label: "Edit",
    description: EDIT_DESC,
    promptSnippet:
      "edit: Change file lines by anchor (replace/append/prepend).",
    parameters: editSchema,

    async execute(_id, params, _signal, _onUpdate, ctx) {
      const abs = resolveToCwd(params.path as string, ctx.cwd);
      const edits = coerceEdits((params as { edits: unknown }).edits);
      if (!edits.length) {
        return {
          content: [{ type: "text", text: "[E_BAD_OP] No edits provided." }],
          isError: true,
          details: { diff: "" } as EditToolDetails,
        };
      }

      let raw: string;
      try {
        await fsAccess(abs, constants.R_OK | constants.W_OK);
        raw = (await fsReadFile(abs)).toString("utf8");
      } catch {
        // Allow file creation via append/prepend without pos.
        const creatable = edits.every((e) =>
          (e.op === "append" || e.op === "prepend") && !e.pos
        );
        if (!creatable) {
          throw new Error(`[E_NO_FILE] File not found: ${params.path}`);
        }
        raw = "";
      }

      const { bom, text } = stripBom(raw);
      const eol = detectEOL(text);
      const orig = toLF(text);

      const { content: result, warnings } = applyEdits(orig, edits);

      if (orig === result) {
        throw new Error(
          `[E_NOOP] No changes made to ${params.path}. Edits produced identical content.`,
        );
      }

      await fsWriteFile(
        abs,
        bom + (eol === "\r\n" ? result.replace(/\n/g, "\r\n") : result),
        "utf8",
      );

      const range = changedLineRange(orig, result);
      const diff = simpleDiff(orig, result);

      let echo = "";
      if (range) {
        const rl = result.split("\n");
        const lo = Math.max(1, range.first - 2);
        const hi = Math.min(rl.length, range.last + 2);
        if (hi - lo + 1 <= ANCHOR_ECHO_MAX) {
          const region = rl.slice(lo - 1, hi).map((l, i) =>
            formatHashLine(lo + i, l)
          ).join("\n");
          echo = `\n\nNew anchors:\n${region}`;
        } else {
          echo = `\n\n(changed lines ${range.first}-${range.last})`;
        }
      }

      const warn = warnings.length
        ? `\n\nWarnings:\n${warnings.join("\n")}`
        : "";

      return {
        content: [{
          type: "text",
          text: `Updated ${params.path}${echo}${warn}`,
        }],
        details: { diff, firstChangedLine: range?.first } as EditToolDetails,
      };
    },
  });

  // ── grep ───────────────────────────────────────────────────────────────────
  // Wrap builtin grep so matches carry edit anchors and can feed `edit` directly,
  // skipping a full-file read on large files.
  const GREP_MATCH_RE = /^(.*?):(\d+): (.*)$/;
  const GREP_CTX_RE = /^(.*?)-(\d+)- (.*)$/;

  pi.registerTool({
    name: "grep",
    label: "Grep",
    description:
      "Search files for a pattern. Each hit is shown as path>>ANCHOR|line. " +
      "Copy ANCHOR straight into the edit tool's pos/end. Use context for surrounding lines.",
    promptSnippet: "grep: Search files; results include edit anchors.",
    parameters: Type.Object({
      pattern: Type.String({ description: "regex or literal" }),
      path: Type.Optional(Type.String({ description: "file or dir (default .)" })),
      glob: Type.Optional(Type.String()),
      ignoreCase: Type.Optional(Type.Boolean()),
      literal: Type.Optional(Type.Boolean()),
      context: Type.Optional(
        Type.Number({ description: "lines before/after each match (default 2)" }),
      ),
      limit: Type.Optional(Type.Number()),
    }),

    async execute(toolCallId, params, signal, onUpdate, ctx) {
      // Default context=2: a lone match line is rarely enough to anchor a
      // range edit, and the model has no other way to see neighbours without
      // a follow-up read.
      const p = { ...params, context: params.context ?? 2 };
      const base = createGrepTool(ctx.cwd);
      const res = await base.execute(toolCallId, p, signal, onUpdate);
      const block = res.content?.find(
        (c): c is { type: "text"; text: string } =>
          c.type === "text" && typeof (c as { text?: unknown }).text === "string",
      );
      if (!block?.text) return res;

      const searchRoot = resolveToCwd(params.path || ".", ctx.cwd);
      let rootIsDir = false;
      try {
        rootIsDir = (await fsStat(searchRoot)).isDirectory();
      } catch { /* treat as file */ }

      const cache = new Map<string, string[] | null>();
      const linesOf = async (rel: string): Promise<string[] | null> => {
        const abs = rootIsDir ? resolvePath(searchRoot, rel) : searchRoot;
        const hit = cache.get(abs);
        if (hit !== undefined) return hit;
        try {
          const raw = (await fsReadFile(abs)).toString("utf8");
          const ls = toLF(stripBom(raw).text).split("\n");
          cache.set(abs, ls);
          return ls;
        } catch {
          cache.set(abs, null);
          return null;
        }
      };

      const out: string[] = [];
      for (const line of block.text.split("\n")) {
        const m = GREP_MATCH_RE.exec(line) ?? GREP_CTX_RE.exec(line);
        if (!m) {
          out.push(line);
          continue;
        }
        const [, rel, ns, shown] = m;
        const n = Number.parseInt(ns, 10);
        if (!Number.isFinite(n) || n < 1) {
          out.push(line);
          continue;
        }
        // Hash the real file line: builtin grep may have truncated `shown`.
        const fl = await linesOf(rel);
        const src = fl?.[n - 1] ?? shown;
        const mark = GREP_MATCH_RE.test(line) ? ">>" : "  ";
        out.push(`${rel}${mark}${n}${computeLineHash(n, src)}|${shown}`);
      }

      return {
        ...res,
        content: res.content.map((c) =>
          c === block ? { ...block, text: out.join("\n") } : c
        ),
      };
    },
  });
}
