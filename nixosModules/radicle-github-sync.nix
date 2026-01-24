# Restricted SSH endpoint for GitHub Actions to sync repos to Radicle
#
# Usage from GitHub Actions:
#   ssh radicle-sync@radicle.thalheim.io
#
# That's it! The SSH key determines which repo to sync. The server fetches
# the latest state from GitHub and pushes to Radicle.
#
# Setup:
#   1. Add repo to services.radicle.githubSync.repos
#   2. Run: clan vars generate
#   3. Run: gh-radicle (in the repo) to set up GitHub secrets/workflow
#
# Flow:
#   1. GitHub Actions triggers sync via SSH (no arguments needed)
#   2. Request sent to socket-activated daemon
#   3. Daemon fetches latest from the configured GitHub repo
#   4. Daemon pushes via rad:// (which signs refs)
#   5. Daemon announces to the network
#   6. Daemon exits after idle timeout
#
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.radicle.githubSync;
  radCfg = config.services.radicle;

  syncPkg = pkgs.callPackage ../pkgs/radicle-github-sync { };

  # Generate authorized_keys entries with per-repo commands
  makeAuthorizedKey =
    repo:
    let
      publicKey = builtins.readFile repo.publicKeyFile;
    in
    ''command="${syncPkg}/bin/radicle-github-sync request ${repo.repoId} ${repo.githubUrl} ${repo.branch}",no-port-forwarding,no-X11-forwarding,no-agent-forwarding,no-pty ${publicKey}'';

  # Sanitize repo name for use as identifier
  sanitizeName = name: builtins.replaceStrings [ "/" "." ] [ "-" "-" ] name;
in
{
  options.services.radicle.githubSync = {
    enable = lib.mkEnableOption "GitHub Actions sync endpoint for Radicle";

    repos = lib.mkOption {
      type = lib.types.listOf (
        lib.types.submodule {
          options = {
            name = lib.mkOption {
              type = lib.types.str;
              description = "Unique name for this sync config (e.g., github-owner-repo)";
              example = "Mic92-dotfiles";
            };
            repoId = lib.mkOption {
              type = lib.types.str;
              description = "Radicle repository ID";
              example = "z2dqRKkK5yu89w3CMX2dVsYrRwvFk";
            };
            githubUrl = lib.mkOption {
              type = lib.types.str;
              description = "GitHub repository URL";
              example = "https://github.com/Mic92/dotfiles";
            };
            branch = lib.mkOption {
              type = lib.types.str;
              default = "main";
              description = "Branch to sync";
              example = "main";
            };
            publicKeyFile = lib.mkOption {
              type = lib.types.path;
              description = "Path to SSH public key file (from clan vars)";
            };
            privateKeyFile = lib.mkOption {
              type = lib.types.path;
              description = "Path to SSH private key file (from clan vars, for gh-radicle to read)";
            };
          };
        }
      );
      default = [ ];
      description = "List of repos to sync from GitHub to Radicle";
    };
  };

  config = lib.mkIf cfg.enable {
    # Generate SSH keys for each repo via clan vars
    clan.core.vars.generators = lib.listToAttrs (
      map (repo: {
        name = "radicle-sync-${sanitizeName repo.name}";
        value = {
          files."ssh-private-key" = {
            secret = true;
          };
          files."ssh-public-key" = { };
          runtimeInputs = [ pkgs.openssh ];
          script = ''
            ssh-keygen -t ed25519 -N "" -f "$out/ssh-private-key" -C "radicle-sync-${repo.name}"
            mv "$out/ssh-private-key.pub" "$out/ssh-public-key"
          '';
        };
      }) cfg.repos
    );

    # Group for socket access
    users.groups.radicle-sync = { };

    # SSH user that receives sync requests
    users.users.radicle-sync = {
      isSystemUser = true;
      group = "radicle-sync";
      home = "/var/lib/radicle-sync";
      shell = "${pkgs.bash}/bin/bash";
      openssh.authorizedKeys.keys = map makeAuthorizedKey cfg.repos;
    };

    systemd.tmpfiles.rules = [
      "d /var/lib/radicle-sync 0750 radicle radicle -"
      "d /var/lib/radicle-sync/repos 0750 radicle radicle -"
    ];

    # Socket for sync requests
    systemd.sockets.radicle-github-sync = {
      description = "Radicle GitHub Sync Socket";
      wantedBy = [ "sockets.target" ];
      socketConfig = {
        ListenStream = "/run/radicle-sync/sync.sock";
        SocketUser = "radicle";
        SocketGroup = "radicle-sync";
        SocketMode = "0660";
        DirectoryMode = "0755";
      };
    };

    # Socket-activated daemon runs as radicle user (has access to keys)
    systemd.services.radicle-github-sync = {
      description = "Radicle GitHub Sync Daemon";
      after = [ "radicle-node.service" ];
      requires = [ "radicle-github-sync.socket" ];
      environment = {
        HOME = "/var/lib/radicle";
        RAD_HOME = "/var/lib/radicle";
      };
      serviceConfig = {
        Type = "simple";
        User = "radicle";
        Group = "radicle";
        LoadCredential = "radicle:${radCfg.privateKeyFile}";
        BindReadOnlyPaths = [
          "${radCfg.configFile}:/var/lib/radicle/config.json"
          "/run/credentials/radicle-github-sync.service/radicle:/var/lib/radicle/keys/radicle"
          "${radCfg.publicKeyFile}:/var/lib/radicle/keys/radicle.pub"
        ];
        StateDirectory = "radicle";
        ReadWritePaths = [ "/var/lib/radicle-sync" ];
        # Exit after idle
        TimeoutStopSec = "5s";
      };
      path = [ syncPkg ];
      script = ''
        exec radicle-github-sync daemon
      '';
    };
  };
}
