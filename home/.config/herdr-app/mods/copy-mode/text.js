// hc's plain-text read joins soft wraps and drops trailing empty rows.
const separators = "!\"#$%&'()*+,-./:;<=>?@[\\]^`{|}~";
const space = /^\p{White_Space}$/u;
const mark = /^\p{Mark}$/u;
const emoji = /^\p{Emoji_Presentation}$/u;
const sessionArg = String.raw`(?!-)[A-Za-z0-9_./~-]{8,}`;
// The command can have any row prefix/label; only its own span is selected.
const agentCommand = new RegExp(
  String
    .raw`(?:claude\s+--resume\s+|agy\s+--conversation=|pi\s+--session\s+|opencode\s+-s\s+|codex\s+resume\s+|omp\s+--resume\s+)` +
    String
      .raw`(?:${sessionArg}|<${sessionArg}>|"${sessionArg}"|'${sessionArg}')(?!\S)`,
  "gu",
);
function* resumeCommands(line) {
  for (const match of line.matchAll(agentCommand)) {
    if (match.index > 0 && /[\p{L}\p{N}_]/u.test(line[match.index - 1])) {
      continue;
    }
    yield { index: match.index, length: match[0].length };
  }
}

function* urlMatches(line) {
  const host =
    /^(?:https?:\/\/|(?=www\.))([a-z\d]+(?:-+[a-z\d]+)*(?:\.[a-z\d]+(?:-+[a-z\d]+)*)*)(?::\d+)?(?=[/?#]|$)/i;
  for (
    const match of line.matchAll(
      /(?:^|[^\p{L}\p{N}_@./-])((?:https?:\/\/|www\.)[^\s<>"'`[\]{}]+)/giu,
    )
  ) {
    const raw = match[1];
    let balance = 0;
    for (const ch of raw) {
      if (ch === "(") balance++;
      if (ch === ")") balance--;
    }
    let length = raw.length;
    while (
      length &&
      (/[.,;:!?]/.test(raw[length - 1]) ||
        (raw[length - 1] === ")" && balance < 0))
    ) {
      if (raw[--length] === ")") balance++;
    }
    const value = raw.slice(0, length);
    const authority = host.exec(value);
    if (authority && (!/^www\./i.test(value) || authority[1].includes("."))) {
      yield { index: match.index + match[0].length - raw.length, length };
    }
  }
}

function width(ch) {
  const n = ch.codePointAt(0);
  return emoji.test(ch) || (n >= 0x1100 && (
      n <= 0x115f || n === 0x2329 || n === 0x232a ||
      (n >= 0x2e80 && n <= 0xa4cf && n !== 0x303f) ||
      (n >= 0xac00 && n <= 0xd7a3) || (n >= 0xf900 && n <= 0xfaff) ||
      (n >= 0xfe10 && n <= 0xfe19) || (n >= 0xfe30 && n <= 0xfe6f) ||
      (n >= 0xff01 && n <= 0xff60) || (n >= 0xffe0 && n <= 0xffe6) ||
      (n >= 0x20000 && n <= 0x3fffd)
    ))
    ? 2
    : 1;
}

function cells(text) {
  const result = [];
  let join = false;
  let regional = false;
  let index = 0;
  for (const ch of text) {
    const offset = index;
    index += ch.length;
    const n = ch.codePointAt(0);
    const flag = n >= 0x1f1e6 && n <= 0x1f1ff;
    const prev = result.at(-1);
    if (
      prev && (join || mark.test(ch) || ch === "\u200d" ||
        (n >= 0x1f3fb && n <= 0x1f3ff) || (n >= 0xe0020 && n <= 0xe007f) ||
        (regional && flag))
    ) {
      if (join || ch === "\ufe0f" || ch === "\u20e3") {
        prev.width = Math.max(prev.width, width(ch), 2);
      }
      join = ch === "\u200d";
      regional = false;
      continue;
    }
    result.push({
      index: offset,
      width: width(ch),
      kind: space.test(ch) ? 0 : separators.includes(ch) ? 1 : 2,
    });
    join = false;
    regional = flag;
  }
  return result;
}

export function terminalText(text, cols, total, bounds) {
  const rows = [];
  const streams = [];
  const indents = [];
  for (const line of text.split("\n")) {
    const cellsInLine = [];
    const firstRow = rows.length;
    const indent = /^ */.exec(line)[0].length;
    let row = [];
    rows.push(row);
    indents.push(indent);
    let col = 0;
    for (const cell of cells(line)) {
      if (col + cell.width > cols) {
        row = [];
        rows.push(row);
        indents.push(indent);
        col = 0;
      }
      const point = { row: rows.length - 1, col, ...cell };
      row.push(point);
      cellsInLine.push(point);
      col += cell.width;
    }
    streams.push({
      line,
      cells: cellsInLine,
      firstRow,
      lastRow: rows.length - 1,
    });
  }
  const first = bounds.first ?? bounds.last + 1 - rows.length;
  const physicalRow = (row) =>
    row < Math.max(0, first) || row >= total ? null : rows[row - first] ?? [];
  const rank = (point) => point.row * cols + point.col;
  const cursorRank = (at, row) => {
    const current = row?.find(({ col, width }) =>
      at.col >= col && at.col < col + width
    );
    return rank(current ? { row: at.row, col: current.col } : at);
  };
  const point = (cell) => ({ row: cell.row + first, col: cell.col });
  const wholeRows = (start, end) => ({
    start: { row: start, col: 0 },
    end: { row: end, col: cols - 1 },
  });
  const retained = ({ start, end }) => start.row >= 0 && end.row < total;

  function* ranges(key) {
    if (key === "a{" || key === "i{") {
      const stack = [];
      let previous = null;
      for (const { line, cells } of streams) {
        for (const cell of cells) {
          const top = stack.at(-1);
          if (top && !top.inner) top.inner = cell;
          const ch = line[cell.index];
          if (ch === "{") stack.push({ outer: cell, inner: null });
          if (ch === "}" && top) {
            stack.pop();
            const start = point(top.outer);
            const end = point(cell);
            const selection = key === "a{"
              ? { start, end }
              : top.inner === cell
              ? null
              : { start: point(top.inner), end: point(previous) };
            yield { start, end, selection };
          }
          previous = cell;
        }
      }
      return;
    }
    for (const { line, firstRow, lastRow, cells } of streams) {
      const innerFirst = cells.find(({ kind }) => kind);
      if (key === "il" && innerFirst) {
        yield {
          start: point(innerFirst),
          end: point(cells.findLast(({ kind }) => kind)),
        };
      } else if (key === "al" && cells.length) {
        yield wholeRows(first + firstRow, first + lastRow);
      } else if (key === "aa") {
        let i = 0;
        for (const { index, length } of resumeCommands(line)) {
          while (i < cells.length && cells[i].index < index) i++;
          if (cells[i]?.index !== index) continue;
          const start = point(cells[i]);
          const endIndex = index + length;
          while (i + 1 < cells.length && cells[i + 1].index < endIndex) i++;
          yield { start, end: point(cells[i]) };
        }
      } else if (key === "au") {
        let i = 0;
        for (const { index, length } of urlMatches(line)) {
          while (i < cells.length && cells[i].index < index) i++;
          if (cells[i]?.index !== index) continue;
          const start = point(cells[i]);
          while (i + 1 < cells.length && cells[i + 1].index < index + length) {
            i++;
          }
          yield { start, end: point(cells[i]) };
        }
      }
    }
  }

  function indentation(at) {
    const min = Math.max(first, 0);
    const max = Math.min(first + rows.length, total) - 1;
    const blank = (row) => rows[row - first].every(({ kind }) => !kind);
    let row = Math.max(min, at.row);
    while (row <= max && blank(row)) row++;
    if (row > max) return null;
    const level = indents[row - first];
    const belongs = (n) => blank(n) || indents[n - first] >= level;
    let start = row;
    let end = row;
    while (start > min && belongs(start - 1)) start--;
    while (end < max && belongs(end + 1)) end++;
    return wholeRows(start, end);
  }

  function spans(big) {
    const result = [];
    for (const { cells: stream } of streams) {
      let run;
      for (const cell of stream) {
        const kind = big && cell.kind ? 1 : cell.kind;
        if (!kind) {
          run = null;
          continue;
        }
        const at = point(cell);
        if (run?.kind !== kind) {
          run = { kind, start: at, end: at };
          result.push(run);
        } else {
          run.end = at;
        }
      }
    }
    return result.filter(({ start }) => start.row >= 0);
  }

  const words = spans(false);
  const bigWords = spans(true);
  return {
    row: physicalRow,
    object(at, key) {
      if (key === "ii") return indentation(at);
      if (key === "il" || key === "al") {
        // These line objects are current-line objects and never seek ahead.
        for (const range of ranges(key)) {
          if (at.row >= range.start.row && at.row <= range.end.row) {
            return range;
          }
        }
        return null;
      }
      const pos = cursorRank(at, physicalRow(at.row));
      let containing;
      let next;
      for (const range of ranges(key)) {
        if (!retained(range)) continue;
        const start = rank(range.start);
        if (start <= pos && rank(range.end) >= pos) {
          // Nested braces prefer the innermost enclosing pair.
          if (!containing || start > rank(containing.start)) containing = range;
        } else if (start > pos && (!next || start < rank(next.start))) {
          next = range;
        }
      }
      const found = containing ?? next;
      if (!found) return null;
      return Object.hasOwn(found, "selection") ? found.selection : found;
    },
    motion(at, key) {
      const row = physicalRow(at.row);
      if (!row) return null;
      if (key === "^") {
        return {
          row: at.row,
          col: row.find(({ kind }) => kind)?.col ?? 0,
        };
      }
      if (key === "$") return { row: at.row, col: row.at(-1)?.col ?? 0 };
      if (key === "{" || key === "}") {
        const step = key === "{" ? -1 : 1;
        for (let i = 1; i < 1000; i++) {
          const next = physicalRow(at.row + i * step);
          if (!next) break;
          if (next.every(({ kind }) => !kind)) {
            return { row: at.row + i * step, col: 0 };
          }
        }
        return null;
      }

      const pos = cursorRank(at, row);
      const word = key === key.toUpperCase() ? bigWords : words;
      switch (key.toLowerCase()) {
        case "w":
          return word.find(({ start }) => rank(start) > pos)?.start ?? null;
        case "b":
          return word.findLast(({ start }) => rank(start) < pos)?.start ?? null;
        case "e":
          // A wide cell ends at its lead column for motion purposes.
          return word.find(({ end }) => rank(end) > pos)?.end ?? null;
      }
    },
  };
}
