/**
 * permission-gate user rules — migrated from ~/.pi/agent/permission-gate.json.
 * All rules are argv-based so quoted mentions (e.g. `echo "ssh ..."`) don't
 * trigger them. Built-in duplicates (git checkout ., gh repo/release) dropped.
 */

import { homedir } from "node:os";

// Match `cmd sub1 sub2 ...` anywhere in the pipeline. `sub` may be a
// string (exact) or string[] (any-of).
const is = (pipe: string[][], cmd: string, ...subs: (string | string[])[]) =>
  pipe.some((argv) =>
    argv[0] === cmd &&
    subs.every((s, i) =>
      Array.isArray(s) ? s.includes(argv[i + 1]) : argv[i + 1] === s
    )
  );

// ~/git and ~/src hold too many repos to scan whole-tree; scope to one repo.
const SLOW_ROOTS = new Set(
  ["git", "src"].flatMap((d) => [`${homedir()}/${d}`, `~/${d}`, `$HOME/${d}`]),
);
const stripSlash = (arg: string) => arg.replace(/\/+\.?$/, "");

export default (helpers: any) => ({
  extraRules: [
    {
      label: "scan ~/git or ~/src",
      group: "scan",
      action: "block",
      test: (p: string[][]) =>
        p.some((argv) =>
          helpers.searchPaths(argv).some((a: string) =>
            SLOW_ROOTS.has(stripSlash(a))
          )
        ),
      reason:
        "find/fd/rg/grep across all of ~/git or ~/src is blocked (too many repos). " +
        "Scope the search to a specific repository, e.g. rg foo ~/git/nixpkgs.",
    },
    {
      label: "nix --refresh",
      action: "block",
      test: (p: string[][]) =>
        helpers.anyCmd(p, "nix", (a: string[]) => a.includes("--refresh")),
      reason:
        "nix --refresh is blocked: cache invalidation is not needed in nix — " +
        "to trigger a rebuild, alter the derivation instead.",
    },
    { label: "ssh", test: (p) => is(p, "ssh") },
    { label: "send email", test: (p) => is(p, "msmtp") },

    // Forced rebuilds are usually wrong; edit the nix expression instead.
    // `nix build --rebuild` and `nix-build`/`nix-store --check` do the same.
    {
      label: "nix forced rebuild",
      test: (p) =>
        p.some(
          (a) =>
            (a[0] === "nix" && a.includes("--rebuild")) ||
            ((a[0] === "nix-build" || a[0] === "nix-store") &&
              a.includes("--check")),
        ),
    },
    {
      label: "deploy to machine",
      test: (p) => is(p, "clan", "machines", "update"),
    },

    // `git checkout [<tree-ish>] -- <paths>` resets tracked files.
    {
      label: "git checkout (reset files)",
      test: (p) =>
        p.some((a) => {
          if (a[0] !== "git") return false;
          const i = a.indexOf("checkout");
          const dd = a.indexOf("--", i + 1);
          return i > 0 && dd > i && dd < a.length - 1;
        }),
    },

    // GitHub
    {
      label: "create GitHub issue",
      test: (p) => is(p, "gh", "issue", "create"),
    },
    {
      label: "modify GitHub issue",
      test: (p) => is(p, "gh", "issue", ["close", "delete", "edit", "comment"]),
    },
    { label: "create GitHub PR", test: (p) => is(p, "gh", "pr", "create") },
    {
      label: "modify GitHub PR",
      test: (p) =>
        is(p, "gh", "pr", ["close", "merge", "edit", "comment", "review"]),
    },

    // Gitea
    {
      label: "create Gitea issue/PR",
      test: (p) => is(p, "tea", ["issue", "pr"], "create"),
    },
    {
      label: "modify Gitea issue/PR",
      test: (p) => is(p, "tea", ["issue", "pr"], ["close", "edit"]),
    },
    { label: "Gitea comment", test: (p) => is(p, "tea", "comment") },
  ],
});
