/**
 * block-commands - reject bash tool calls matching BLOCK_RULES before
 * execution. Uses the shared _shell tokenizer so quoting/pipelines/env
 * prefixes/command substitutions are handled and rules see argv, not raw
 * strings.
 */
import {
  type ExtensionAPI,
  isToolCallEventType,
} from "@mariozechner/pi-coding-agent";
import { simpleCommands } from "./_shell.ts";

const HOME = process.env.HOME;

const WHOLE_TREE_ROOTS = new Set(
  ["/", "~", "$HOME", HOME].filter((v): v is string => !!v),
);
const NIX_ROOTS = new Set(["/nix", "/nix/store"]);

// `~/`, `$HOME/` -> their bare root; `/` stays `/`.
const normalizeRoot = (arg: string) =>
  arg === "/" ? "/" : arg.replace(/\/+$/, "");

// Arguments the command treats as search paths, so patterns and flags are
// not mistaken for paths. find: paths precede the first expression. fd/rg/
// grep: first positional is the pattern (unless -e/-f supplies it), rest are
// paths.
function searchPaths(argv: string[]): string[] {
  const [cmd, ...args] = argv;

  if (cmd === "find") {
    const paths: string[] = [];
    for (const arg of args) {
      if (/^-[HLP]$/.test(arg)) continue; // symlink-mode flags precede paths
      if (arg.startsWith("-") || arg === "(" || arg === "!") break;
      paths.push(arg);
    }
    return paths;
  }

  if (cmd !== "fd" && cmd !== "rg" && cmd !== "grep") return [];

  // Flags that carry the pattern, so every positional arg is a path.
  const patternFlags = /^(-e|-f|--regexp(=.*)?|--file(=.*)?)$/;
  let patternFromFlag = false;
  let afterDoubleDash = false;
  const positionals: string[] = [];
  for (const arg of args) {
    if (!afterDoubleDash) {
      if (arg === "--") {
        afterDoubleDash = true;
        continue;
      }
      if (arg.startsWith("-")) {
        if (patternFlags.test(arg)) patternFromFlag = true;
        continue;
      }
    }
    positionals.push(arg);
  }
  return patternFromFlag ? positionals : positionals.slice(1);
}

const scansRoot = (argv: string[], roots: Set<string>) =>
  searchPaths(argv).some((arg) => roots.has(normalizeRoot(arg)));

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
