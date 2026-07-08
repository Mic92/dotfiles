/**
 * _shell - minimal bash tokenizer shared by pi extensions.
 *
 * Splits a command string into simple commands (argv arrays) so extensions
 * can inspect what programs actually run, including inside pipelines,
 * command substitutions and process substitutions. Not a full shell parser:
 * heredoc bodies are skipped, arithmetic is opaque, expansions are kept
 * literal (${VAR} normalized to $VAR so rules can match "$HOME").
 */

export type Token =
  | { type: "word"; value: string }
  | { type: "op"; value: string }
  | { type: "sub"; value: string }; // inner script of $(...) or `...`

// Multi-char operators first so e.g. "&&" is not split into "&" "&".
const OPERATORS = [
  "<<<",
  "<<-",
  "<<",
  ">>",
  "&&",
  "||",
  ";;",
  "|&",
  "&>",
  ">&",
  "<&",
  "|",
  "&",
  ";",
  "(",
  ")",
  "<",
  ">",
  "\n",
];

// Redirects do not end a simple command; the word after them is a target
// (or herestring data), not an argument.
const REDIRECT_OPS = new Set([">", ">>", "<", ">&", "<&", "&>", "<<<"]);

/** Tokenize a shell command string into words, operators and substitutions. */
export function tokenize(input: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;
  let word = "";
  let inWord = false;
  // Delimiters seen after << / <<- whose bodies still need to be skipped
  // once the current line ends.
  const pendingHeredocs: { delim: string; stripTabs: boolean }[] = [];
  let expectHeredoc: { stripTabs: boolean } | null = null;

  const pushWord = () => {
    if (!inWord) return;
    if (expectHeredoc) {
      pendingHeredocs.push({ delim: word, stripTabs: expectHeredoc.stripTabs });
      expectHeredoc = null;
    } else {
      tokens.push({ type: "word", value: word });
    }
    word = "";
    inWord = false;
  };

  // `start` must be right after a newline; returns index after the last body.
  const skipHeredocBodies = (start: number): number => {
    let j = start;
    for (const { delim, stripTabs } of pendingHeredocs) {
      while (j < input.length) {
        let lineEnd = input.indexOf("\n", j);
        if (lineEnd === -1) lineEnd = input.length;
        let line = input.slice(j, lineEnd);
        if (stripTabs) line = line.replace(/^\t+/, "");
        j = lineEnd + 1;
        if (line === delim) break;
      }
    }
    pendingHeredocs.length = 0;
    return Math.min(j, input.length);
  };

  // Reads ${NAME} at index of "$"; simple names are normalized to $NAME.
  const readBraceVar = (start: number): [string, number] => {
    const end = input.indexOf("}", start + 2);
    if (end === -1) return [input.slice(start), input.length];
    const name = input.slice(start + 2, end);
    const text = /^[A-Za-z_][A-Za-z0-9_]*$/.test(name)
      ? `$${name}`
      : input.slice(start, end + 1);
    return [text, end + 1];
  };

  // Returns index after a quote-delimited region starting at `start`
  // (the opening quote), honoring backslash escapes.
  const skipQuoted = (start: number, quote: string): number => {
    let j = start + 1;
    while (j < input.length && input[j] !== quote) {
      j += input[j] === "\\" ? 2 : 1;
    }
    return Math.min(j + 1, input.length);
  };

  // Reads a $( ... ) starting at index of "(", returns [inner, nextIndex].
  // Skips quoted regions, backticks and comments so a `)` inside them does
  // not affect paren depth counting.
  const readDollarParen = (start: number): [string, number] => {
    let depth = 1;
    let j = start + 1;
    let inner = "";
    let atWordStart = true;
    while (j < input.length && depth > 0) {
      const c = input[j];
      if (c === "'" || c === '"' || c === "`") {
        // no backslash escapes inside single quotes
        const end = c === "'" ? input.indexOf("'", j + 1) : -1;
        const stop = c === "'"
          ? (end === -1 ? input.length : end + 1)
          : skipQuoted(j, c);
        inner += input.slice(j, stop);
        j = stop;
        atWordStart = false;
        continue;
      }
      if (c === "#" && atWordStart) {
        let end = input.indexOf("\n", j);
        if (end === -1) end = input.length;
        j = end;
        continue;
      }
      if (c === "\\") {
        inner += input.slice(j, j + 2);
        j += 2;
        atWordStart = false;
        continue;
      }
      atWordStart = " \t\n;|&(".includes(c);
      if (c === "(") depth++;
      else if (c === ")") {
        depth--;
        if (depth === 0) {
          j++;
          break;
        }
      }
      inner += c;
      j++;
    }
    return [inner, j];
  };

  // Reads a `...` starting at index after opening backtick.
  const readBacktick = (start: number): [string, number] => {
    let j = start;
    let inner = "";
    while (j < input.length) {
      const c = input[j];
      if (c === "\\" && j + 1 < input.length) {
        // \` \\ \$ unescape inside backticks
        const next = input[j + 1];
        inner += "`\\$".includes(next) ? next : c + next;
        j += 2;
        continue;
      }
      if (c === "`") {
        j++;
        break;
      }
      inner += c;
      j++;
    }
    return [inner, j];
  };

  const pushSub = (inner: string, placeholder: string, next: number) => {
    tokens.push({ type: "sub", value: inner });
    word += placeholder;
    inWord = true;
    i = next;
  };

  while (i < input.length) {
    const c = input[i];

    // Comment: only when not inside a word.
    if (c === "#" && !inWord) {
      while (i < input.length && input[i] !== "\n") i++;
      continue;
    }

    // Whitespace (newline is an operator, handled below).
    if (c === " " || c === "\t" || c === "\r") {
      pushWord();
      i++;
      continue;
    }

    // Backslash outside quotes (\<newline> is a line continuation).
    if (c === "\\") {
      if (i + 1 < input.length && input[i + 1] !== "\n") {
        word += input[i + 1];
        inWord = true;
      }
      i += 2;
      continue;
    }

    // Single quotes: literal.
    if (c === "'") {
      const end = input.indexOf("'", i + 1);
      const stop = end === -1 ? input.length : end;
      word += input.slice(i + 1, stop);
      inWord = true;
      i = end === -1 ? input.length : end + 1;
      continue;
    }

    // Double quotes.
    if (c === '"') {
      inWord = true;
      i++;
      while (i < input.length && input[i] !== '"') {
        const d = input[i];
        if (d === "\\" && i + 1 < input.length) {
          const next = input[i + 1];
          if ('"\\$`'.includes(next)) word += next;
          else if (next !== "\n") word += d + next; // \<newline> continues line
          i += 2;
        } else if (d === "$" && input[i + 1] === "(") {
          const [inner, next] = readDollarParen(i + 1);
          pushSub(inner, "$(...)", next);
        } else if (d === "$" && input[i + 1] === "{") {
          const [text, next] = readBraceVar(i);
          word += text;
          i = next;
        } else if (d === "`") {
          const [inner, next] = readBacktick(i + 1);
          pushSub(inner, "$(...)", next);
        } else {
          word += d;
          i++;
        }
      }
      i++; // closing quote
      continue;
    }

    // Command substitution and arithmetic outside quotes.
    if (c === "$" && input[i + 1] === "(") {
      if (input[i + 2] === "(") {
        // $(( ... )): opaque word content
        const end = input.indexOf("))", i + 3);
        const stop = end === -1 ? input.length : end + 2;
        word += input.slice(i, stop);
        inWord = true;
        i = stop;
      } else {
        const [inner, next] = readDollarParen(i + 1);
        pushSub(inner, "$(...)", next);
      }
      continue;
    }
    if (c === "`") {
      const [inner, next] = readBacktick(i + 1);
      pushSub(inner, "$(...)", next);
      continue;
    }

    // ${VAR} outside quotes: normalize to $VAR.
    if (c === "$" && input[i + 1] === "{") {
      const [text, next] = readBraceVar(i);
      word += text;
      inWord = true;
      i = next;
      continue;
    }

    // Process substitution <(...) / >(...): scan contents too.
    if ((c === "<" || c === ">") && input[i + 1] === "(") {
      const [inner, next] = readDollarParen(i + 1);
      pushSub(inner, `${c}(...)`, next);
      continue;
    }

    // Operators.
    const op = OPERATORS.find((o) => input.startsWith(o, i));
    if (op) {
      // A pure-digit word directly before a redirect is a file descriptor
      // (e.g. 2>&1), not an argument.
      if (inWord && /^\d+$/.test(word) && /^[<>]/.test(op)) {
        word = "";
        inWord = false;
      }
      pushWord();
      if (op === "\n") {
        tokens.push({ type: "op", value: ";" });
        i++;
        if (pendingHeredocs.length) i = skipHeredocBodies(i);
        continue;
      }
      tokens.push({ type: "op", value: op });
      if (op === "<<" || op === "<<-") {
        expectHeredoc = { stripTabs: op === "<<-" };
      }
      i += op.length;
      continue;
    }

    // Regular character ($VAR stays literal).
    word += c;
    inWord = true;
    i++;
  }
  pushWord();
  return tokens;
}

/**
 * Split a command string into simple commands (argv arrays), breaking on
 * operators and recursing into command substitutions. Leading VAR=value
 * prefixes are dropped so argv[0] is the program.
 */
export function simpleCommands(command: string): string[][] {
  const commands: string[][] = [];
  let current: string[] = [];
  let skipRedirectTarget = false;

  const flush = () => {
    if (current.length) commands.push(current);
    current = [];
  };

  for (const token of tokenize(command)) {
    if (token.type === "op") {
      if (REDIRECT_OPS.has(token.value)) {
        skipRedirectTarget = true;
      } else {
        skipRedirectTarget = false;
        flush();
      }
      continue;
    }
    if (token.type === "sub") {
      commands.push(...simpleCommands(token.value));
      continue;
    }
    if (skipRedirectTarget) {
      skipRedirectTarget = false;
      continue;
    }
    if (current.length === 0 && /^[A-Za-z_][A-Za-z0-9_]*=/.test(token.value)) {
      continue;
    }
    current.push(token.value);
  }
  flush();
  return commands;
}
