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

const SCAN_COMMANDS = new Set(["find", "fd", "rg", "grep"]);
const WHOLE_TREE_ROOTS = new Set(
  ["/", "~", "$HOME", HOME].filter((v): v is string => !!v),
);
const NIX_ROOTS = new Set(["/nix", "/nix/store"]);

// `~/`, `$HOME/` -> their bare root. `/` stays `/`; other all-slash args
// (e.g. a `//` grep pattern) normalize to "" and never match a root.
const normalizeRoot = (arg: string) =>
  arg === "/" ? "/" : arg.replace(/\/+$/, "");

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
