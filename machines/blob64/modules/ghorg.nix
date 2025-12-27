{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.services.ghorg;

  backupType = lib.types.submodule {
    options = {
      target = lib.mkOption {
        type = lib.types.str;
        description = "Organization or user name to backup";
      };
      scm = lib.mkOption {
        type = lib.types.enum [
          "github"
          "gitea"
        ];
        default = "github";
        description = "SCM type";
      };
      cloneType = lib.mkOption {
        type = lib.types.enum [
          "org"
          "user"
        ];
        default = "org";
        description = "Clone type (org or user)";
      };
      baseUrl = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Base URL for self-hosted instances";
      };
      tokenFile = lib.mkOption {
        type = lib.types.path;
        description = "Path to token file";
      };
      matchRegex = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Only clone repos matching this regex";
      };
      descriptionFilter = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Only clone repos with this string in description (uses gh API)";
      };
      skipForks = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Skip forked repositories";
      };
    };
  };

  mkGhorgScript =
    name: backup:
    let
      extraArgs = lib.optionals (backup.matchRegex != null) [
        "--match-regex"
        backup.matchRegex
      ];
    in
    if backup.descriptionFilter != null then
      pkgs.writeShellScript "ghorg-${name}" ''
        set -euo pipefail
        export GHORG_ABSOLUTE_PATH_TO_CLONE_TO="${cfg.path}"
        export GHORG_SCM_TYPE="${backup.scm}"
        export GHORG_CLONE_TYPE="${backup.cloneType}"

        TOKEN=$(cat "${backup.tokenFile}")
        export GHORG_GITHUB_TOKEN="$TOKEN"
        export GH_TOKEN="$TOKEN"

        # Query GitHub API for repos with filter in description (excluding archived)
        repos=$(${pkgs.gh}/bin/gh search repos --owner ${backup.target} --json name,description,isArchived --limit 200 \
          | ${pkgs.jq}/bin/jq -r '.[] | select(.description | test("${backup.descriptionFilter}"; "i")) | select(.isArchived == false) | .name')

        if [ -z "$repos" ]; then
          echo "No repos found with '${backup.descriptionFilter}' in description"
          exit 0
        fi

        repofile=$(mktemp)
        echo "$repos" > "$repofile"
        trap "rm -f $repofile" EXIT

        exec ${pkgs.ghorg}/bin/ghorg clone ${backup.target} \
          --backup \
          --prune \
          --prune-no-confirm \
          --skip-archived \
          --skip-forks \
          --target-repos-path "$repofile"
      ''
    else
      pkgs.writeShellScript "ghorg-${name}" ''
        set -euo pipefail
        export GHORG_ABSOLUTE_PATH_TO_CLONE_TO="${cfg.path}"
        export GHORG_SCM_TYPE="${backup.scm}"
        export GHORG_CLONE_TYPE="${backup.cloneType}"
        ${lib.optionalString (backup.baseUrl != null) ''export GHORG_SCM_BASE_URL="${backup.baseUrl}"''}

        TOKEN=$(cat "${backup.tokenFile}")
        ${
          if backup.scm == "gitea" then
            ''export GHORG_GITEA_TOKEN="$TOKEN"''
          else
            ''export GHORG_GITHUB_TOKEN="$TOKEN"''
        }

        exec ${pkgs.ghorg}/bin/ghorg clone ${backup.target} \
          --backup \
          --prune \
          --prune-no-confirm \
          --skip-archived \
          ${lib.optionalString backup.skipForks "--skip-forks"} \
          ${lib.escapeShellArgs extraArgs}
      '';

  mkService = name: backup: {
    description = "Backup ${backup.target} repos";
    path = [ pkgs.gitMinimal ];
    serviceConfig = {
      Type = "oneshot";
      RuntimeDirectory = "ghorg";
      Environment = "HOME=/run/ghorg";
      ExecStart = mkGhorgScript name backup;
    };
  };

  mkTimer = name: _backup: {
    description = "Timer for ${name} backup";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "daily";
      RandomizedDelaySec = "1h";
      Persistent = true;
    };
  };
in
{
  options.services.ghorg = {
    path = lib.mkOption {
      type = lib.types.path;
      default = "/zdata/ghorg";
      description = "Path to store backups";
    };
    backups = lib.mkOption {
      type = lib.types.attrsOf backupType;
      default = { };
      description = "Backup configurations";
    };
  };

  config = lib.mkIf (cfg.backups != { }) {
    # Tokens for GitHub and Gitea
    clan.core.vars.generators.ghorg-github-token = {
      files.token = { };
      prompts.token.description = "GitHub token for ghorg (needs repo read access)";
      script = "cp $prompts/token $out/token";
    };

    clan.core.vars.generators.ghorg-gitea-token = {
      files.token = { };
      prompts.token.description = "Gitea token for git.clan.lol (needs repo read access)";
      script = "cp $prompts/token $out/token";
    };

    systemd.tmpfiles.rules = [
      "d ${cfg.path} 0755 root root -"
    ];

    systemd.services = lib.mapAttrs' (
      name: backup: lib.nameValuePair "ghorg-${name}" (mkService name backup)
    ) cfg.backups;

    systemd.timers = lib.mapAttrs' (
      name: backup: lib.nameValuePair "ghorg-${name}" (mkTimer name backup)
    ) cfg.backups;
  };
}
