#!/usr/bin/env bash

set -xeu -o pipefail

cleanupHooks=()
runCleanupHooks() {
  local hook
  for hook in "${cleanupHooks[@]}"; do "$hook" || true; done
}
trap runCleanupHooks EXIT

createPR() {
  local msg firstLine rest tmpdir targetBranch remoteName branch
  branch=$1
  targetBranch=$2
  tmpdir=$(mktemp -d)
  readonly createPRTmpdir=$tmpdir
  cleanupPRTempDir() { rm -rf "$createPRTmpdir"; }
  cleanupHooks+=(cleanupPRTempDir)
  remoteName=origin
  if [[ $(git remote) =~ upstream ]]; then
    remoteName=upstream
  fi
  git log --reverse --pretty="format:%s%n%n%b%n%n" "$remoteName/$targetBranch..HEAD" >"$tmpdir/COMMIT_EDITMSG"
  ${EDITOR:-vim} "$tmpdir/COMMIT_EDITMSG"
  msg=$(<"$tmpdir/COMMIT_EDITMSG")
  firstLine=${msg%%$'\n'*}
  rest=${msg#*$'\n'}
  if [[ $firstLine == "$rest" ]]; then
    rest=""
  fi
  gh pr create --title "$firstLine" --body "$rest" --base "$targetBranch" --head "$branch" --label merge-queue
}

runTreefmt() {
  local hasTreefmt targetBranch formatter
  targetBranch=$1
  if command -v treefmt 2>/dev/null; then
    treefmt --fail-on-change
    return 0
  fi
  # detect treefmt embedded in a flake
  # shellcheck disable=SC2016
  hasTreefmt='(val: val ? ${builtins.currentSystem} && val.${builtins.currentSystem}.name == "treefmt")'
  if [[ $(nix eval .#formatter --impure --apply "$hasTreefmt" 2>/dev/null || true) != true ]]; then
    return 0
  fi

  # shellcheck disable=SC2016
  formatter=$(nix build -o .git/treefmt '.#formatter.${builtins.currentSystem}' --print-out-paths)

  if "$formatter/bin/treefmt" --fail-on-change; then
    return 0
  fi

  git absorb --force --and-rebase --base "origin/$targetBranch"
  lazygit
  return 1
}

main() {
  local targetBranch branch isOpen

  if command -v merge-after-ci 2>/dev/null; then # clan-project
    exec merge-after-ci --no-review "$@"
  fi

  targetBranch=$(gh repo view --json defaultBranchRef --jq .defaultBranchRef.name)

  runTreefmt "$targetBranch"

  git pull --rebase origin "$targetBranch"

  branch=merge-when-green-$(id -un)
  isOpen=$(gh pr view --json state --template '{{.state}}' "$branch" || true)
  if [[ $isOpen == "OPEN" ]]; then
    gh pr checks "$targetBranch" || true
  fi
  git push --force origin "HEAD:$branch"

  if [[ $isOpen != "OPEN" ]]; then
    createPR "$branch" "$targetBranch"
  fi
}

main "$@"
