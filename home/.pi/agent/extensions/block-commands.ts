/**
 * block-commands - reject bash tool calls matching BLOCK_RULES before
 * execution. Uses the shared _shell parser so quoting/env prefixes/command
 * substitutions are handled and rules see pipelines of argv arrays, not raw
 * strings.
 */
import {
  type ExtensionAPI,
  isToolCallEventType,
} from "@mariozechner/pi-coding-agent";
import { pipelines, simpleCommands } from "./_shell.ts";

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
  // Checked once per pipeline; a pipeline is the list of argvs joined by
  // pipes, so rules can reason about producer/consumer combinations.
  matches: (pipeline: string[][]) => boolean;
  reason: string;
}

const BLOCK_RULES: BlockRule[] = [
  {
    matches: (pipeline) => pipeline.some((argv) => scansRoot(argv, NIX_ROOTS)),
    reason: "find/fd/rg/grep on /nix/store is blocked (millions of files). " +
      "Use nix-locate to find store paths, or inspect env vars like " +
      "$buildInputs / $NIX_CFLAGS_COMPILE / $PKG_CONFIG_PATH inside a shell.",
  },
  {
    matches: (pipeline) =>
      pipeline.some((argv) => scansRoot(argv, WHOLE_TREE_ROOTS)),
    reason:
      "find/fd/rg/grep on / or $HOME is blocked (too slow). Scope to a subdir.",
  },
  {
    // head/tail inside the quoted task of `pueue add -- '... | tail'`
    // (re-parsed as shell). Piping pueue's own output to head/tail is fine.
    matches: (pipeline) =>
      pipeline.some((c) =>
        c[0] === "pueue" && c[1] === "add" &&
        c.slice(2).some((arg) =>
          simpleCommands(arg).some((sub) =>
            sub[0] === "tail" || sub[0] === "head"
          )
        )
      ),
    reason: "Do not head/tail inside a pueue task; it hides live output. " +
      "Queue the command without head/tail, stream it with `pueue follow`, " +
      "and use `pueue log --lines N` to view the end of the output.",
  },
];

export default function (pi: ExtensionAPI) {
  pi.on("tool_call", (event) => {
    if (!isToolCallEventType("bash", event)) return;

    // Pipelines as argv lists, recursing into command/process substitutions.
    const collect = (script: string): string[][][] =>
      pipelines(script).flatMap((p) => [
        p.map((c) => c.argv).filter((argv) => argv.length),
        ...p.flatMap((c) => c.subs.flatMap(collect)),
      ]).filter((p) => p.length);

    for (const pipeline of collect(event.input.command)) {
      for (const rule of BLOCK_RULES) {
        if (rule.matches(pipeline)) {
          return { block: true, reason: rule.reason };
        }
      }
    }
  });
}
