/**
 * block-commands - reject bash tool calls matching BLOCK_RULES before
 * execution. Tokenizes with shell-quote so quoting/pipelines/env prefixes
 * are handled and rules see argv, not raw strings.
 */
import {
  type ExtensionAPI,
  isToolCallEventType,
} from "@mariozechner/pi-coding-agent";
import { parse } from "shell-quote";

const HOME = process.env.HOME;

const SCAN_COMMANDS = new Set(["find", "fd", "rg", "grep"]);
const WHOLE_TREE_ROOTS = new Set(
  ["/", "~", "$HOME", HOME].filter((v): v is string => !!v),
);
const NIX_ROOTS = new Set(["/nix", "/nix/store"]);

// `~/`, `$HOME/`, `//` -> their bare root.
const normalizeRoot = (arg: string) => arg.replace(/\/+$/, "") || "/";

const scansRoot = (argv: string[], roots: Set<string>) =>
  SCAN_COMMANDS.has(argv[0]) &&
  argv.slice(1).some((arg) => roots.has(normalizeRoot(arg)));

interface BlockRule {
  matches: (argv: string[]) => boolean;
  reason: string;
}

const BLOCK_RULES: BlockRule[] = [
  {
    matches: (argv) => scansRoot(argv, NIX_ROOTS),
    reason: "find/fd/rg/grep on /nix/store is blocked (millions of files). " +
      "Use nix-locate to find store paths, or inspect env vars like " +
      "$buildInputs / $NIX_CFLAGS_COMPILE / $PKG_CONFIG_PATH inside a shell.",
  },
  {
    matches: (argv) => scansRoot(argv, WHOLE_TREE_ROOTS),
    reason:
      "find/fd/rg/grep on / or $HOME is blocked (too slow). Scope to a subdir.",
  },
];

// Split token stream into simple commands (argv arrays), breaking on
// operators. Leading VAR=value prefixes are dropped so argv[0] is the program.
function simpleCommands(command: string): string[][] {
  const commands: string[][] = [];
  let current: string[] = [];

  // Keep `$HOME` literal instead of expanding to "".
  for (const token of parse(command, (name) => `$${name}`)) {
    if (typeof token === "object" && "op" in token) {
      if (current.length) commands.push(current);
      current = [];
      continue;
    }
    const word = typeof token === "string" ? token : String(token);
    if (current.length === 0 && /^[A-Za-z_][A-Za-z0-9_]*=/.test(word)) continue;
    current.push(word);
  }
  if (current.length) commands.push(current);
  return commands;
}

export default function (pi: ExtensionAPI) {
  pi.on("tool_call", async (event) => {
    if (!isToolCallEventType("bash", event)) return;

    for (const argv of simpleCommands(event.input.command)) {
      for (const rule of BLOCK_RULES) {
        if (rule.matches(argv)) {
          return { block: true, reason: rule.reason };
        }
      }
    }
  });
}
