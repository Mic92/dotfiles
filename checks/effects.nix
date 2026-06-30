# Dogfood for nixbot effects: exercises the hercules-style
# secrets file and the current-task state API.
{ inputs, self }:
_args:
let
  pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;

  # Scheduled effect operating on a fresh clone. The GitToken secret
  # authenticates git push (token in the origin URL) and gh (GH_TOKEN).
  mkRepoEffect =
    name: script:
    pkgs.runCommand "effect-${name}"
      {
        nativeBuildInputs = [
          pkgs.cacert
          pkgs.git
          pkgs.gh
          pkgs.jq
          pkgs.nix
          pkgs.openssh
        ];
        # mkEffect JSON-encodes secretsMap; raw derivations must too.
        secretsMap = builtins.toJSON { git.type = "GitToken"; };
        # The sandbox does not inherit the host HOME.
        HOME = "/build";
      }
      ''
        set -euo pipefail
        # The sandbox nix has no experimental features enabled; child
        # nix invocations (updater, nix-update) inherit this.
        export NIX_CONFIG="experimental-features = nix-command flakes"
        token=$(jq -r '.git.data.token' "$HERCULES_CI_SECRETS_JSON")
        export GH_TOKEN="$token"
        git config --global user.name "dotfiles-bot"
        git config --global user.email "dotfiles-bot@users.noreply.github.com"
        git config --global safe.directory '*'
        git clone --recurse-submodules \
          "https://x-access-token:$token@github.com/Mic92/dotfiles" repo
        cd repo
        ${script}
      '';
in
{
  onPush.default.outputs.effects = {
    state-test =
      pkgs.runCommand "effect-state-test"
        {
          nativeBuildInputs = [
            # cacert's setup hook exports SSL_CERT_FILE (hercules mkEffect
            # ships it as a default input for the same reason).
            pkgs.cacert
            pkgs.curl
            pkgs.jq
          ];
          rev = self.rev or "dirty";
        }
        ''
          token=$(jq -r '.["hercules-ci"].data.token' "$HERCULES_CI_SECRETS_JSON")
          base="$HERCULES_CI_API_BASE_URL/api/v1/current-task/state/dogfood/data"
          echo "project: $HERCULES_CI_PROJECT_PATH rev: $rev"
          echo "state-test $rev" | curl -fsS -X PUT --data-binary @- \
            -H "Authorization: Bearer $token" "$base"
          got=$(curl -fsS -H "Authorization: Bearer $token" "$base")
          echo "state roundtrip: $got"
          [ "$got" = "state-test $rev" ]
        '';

    sandbox-test =
      pkgs.runCommand "effect-sandbox-test"
        {
          nativeBuildInputs = [
            pkgs.jq
            pkgs.nix
          ];
          # mkEffect JSON-encodes secretsMap; raw derivations must too.
          secretsMap = builtins.toJSON { git.type = "GitToken"; };
        }
        ''
          [ "$HOME" = /homeless-shelter ]
          [ "$(id -u)" = 0 ]
          [ "$NIX_REMOTE" = daemon ]
          # Private per-run daemon proxy (untrusted nix-daemon).
          nix --extra-experimental-features nix-command store info
          # secretsMap selection: exactly the requested secrets, and
          # the GitToken entry carries a usable token.
          jq -e 'keys == ["git", "hercules-ci"]' "$HERCULES_CI_SECRETS_JSON"
          jq -er '.git.data.token | length > 0' "$HERCULES_CI_SECRETS_JSON"
          echo sandbox checks passed
        '';
  };

  # Hourly heartbeat (deterministic pseudo-random minute) exercising
  # the scheduled-effects loop; stores its timestamp via the state API.
  onSchedule.heartbeat = {
    when = { };
    outputs.effects.heartbeat =
      pkgs.runCommand "effect-heartbeat"
        {
          nativeBuildInputs = [
            pkgs.cacert
            pkgs.curl
            pkgs.jq
          ];
        }
        ''
          token=$(jq -r '.["hercules-ci"].data.token' "$HERCULES_CI_SECRETS_JSON")
          date -u +%FT%TZ | curl -fsS -X PUT --data-binary @- \
            -H "Authorization: Bearer $token" \
            "$HERCULES_CI_API_BASE_URL/api/v1/current-task/state/heartbeat/data"
          echo heartbeat stored
        '';
  };

  # Daily package updates; one PR per changed package.
  onSchedule.update-packages = {
    when = {
      hour = 3;
      minute = 0;
    };
    outputs.effects.update-packages = mkRepoEffect "update-packages" ''
      nix run .#updater -- --pr
    '';
  };

  # Daily zsh submodule bump on a single branch and PR.
  onSchedule.update-submodules = {
    when = {
      hour = 2;
      minute = 51;
    };
    outputs.effects.update-submodules = mkRepoEffect "update-submodules" ''
      git submodule update --init --recursive
      git submodule update --recursive --remote
      if git diff --quiet; then
        echo "no submodule changes"
        exit 0
      fi
      branch=update-zsh-modules
      git checkout -b "$branch"
      git commit -am "Update zsh modules"
      git push -f origin "$branch"
      if ! gh pr view "$branch" >/dev/null 2>&1; then
        gh pr create --head "$branch" --title "Update zsh modules" \
          --body "Automated zsh submodule update." --label auto-merge
      fi
    '';
  };
}
