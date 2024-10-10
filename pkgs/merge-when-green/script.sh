#!/usr/bin/env bash

set -eu -o pipefail

if command -v merge-after-ci 2>/dev/null; then # clan-project
  command merge-after-ci --no-review "$@"
  return
fi
if command -v treefmt 2>/dev/null && ! treefmt --fail-on-change; then
  return
fi

targetBranch=$(gh repo view --json defaultBranchRef --jq .defaultBranchRef.name)

git pull --rebase origin "$targetBranch"

# detect treefmt embedded in a flake
# shellcheck disable=SC2016
readonly has_treefmt='(val: val ? ${builtins.currentSystem} && val.${builtins.currentSystem}.name == "treefmt")'
if [[ $(nix eval .#formatter --impure --apply "$has_treefmt") == true ]]; then
  # shellcheck disable=SC2016
  formatter=$(nix build -o .git/treefmt '.#formatter.${builtins.currentSystem}' --print-out-paths)
  if ! "$formatter/bin/treefmt" --fail-on-change; then
    git absorb --force --and-rebase --base "origin/$targetBranch"
    lazygit
    return
  fi
fi
branch=merge-when-green-$(id -un)
is_open=$(gh pr view --json state --template '{{.state}}' "$branch")
if [[ $is_open == "OPEN" ]]; then
  gh pr checks "$targetBranch"
fi
git push --force origin "HEAD:$branch"
remoteName=origin
if [[ $(git remote) =~ upstream ]]; then
  remoteName=upstream
fi

if [[ $is_open != "OPEN" ]]; then
  # BUFFER is an internal variable used by edit-command-line
  # We fill it with commit subject and body seperated by newlines
  tmpdir=$(mktemp -d)
  trap 'rm -rf "$tmpdir"' EXIT
  git log --reverse --pretty="format:%s%n%n%b%n%n" "$remoteName/$targetBranch..HEAD" >"$tmpdir/COMMIT_EDITMSG"
  ${EDITOR:-vim} "$tmpdir/COMMIT_EDITMSG"
  msg=$(<"$tmpdir/COMMIT_EDITMSG")
  firstLine=${msg%%$'\n'*}
  rest=${msg#*$'\n'}
  if [[ $firstLine == "$rest" ]]; then
    rest=""
  fi
  gh pr create --title "$firstLine" --body "$rest" --base "$targetBranch" --head "$branch" --label merge-queue
fi
