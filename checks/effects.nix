# Dogfood for nixbot effects: exercises the hercules-style
# secrets file and the current-task state API.
{ inputs, self }:
_args: {
  onPush.default.outputs.effects = {
    state-test =
      let
        pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
      in
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
      let
        pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
      in
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
      let
        pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
      in
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
}
