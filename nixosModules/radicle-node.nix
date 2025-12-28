{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.radicle;
in
{
  options = {
    services.radicle.seedRepositories = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = ''
        List of Radicle repository IDs to automatically seed.
        These will also be pinned in the web interface.
        Leave empty to automatically discover and seed all repos from followed users.
      '';
      example = [
        "rad:z2dqRKkK5yu89w3CMX2dVsYrRwvFk"
        "rad:z3gpeDzWxqV8iBEN8RcJNZEPVWmJf"
      ];
    };

    services.radicle.autoSeedFollowed = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Automatically discover and seed all repositories from followed users.
        When enabled, the node will periodically check for new repos from followed users
        and automatically seed them.
      '';
    };
  };

  config = {
    # SSH key generation via clan vars
    clan.core.vars.generators.radicle = {
      files.ssh-private-key = {
        secret = true;
        owner = "radicle";
      };
      files.ssh-public-key = {
        secret = false;
      };
      runtimeInputs = with pkgs; [ openssh ];
      script = ''
        ssh-keygen -t ed25519 -N "" -f $out/ssh-private-key -C "radicle@${config.networking.hostName}"
        ssh-keygen -y -f $out/ssh-private-key > $out/ssh-public-key
      '';
    };

    # Radicle node (no httpd/webui)
    services.radicle = {
      enable = true;
      privateKeyFile = config.clan.core.vars.generators.radicle.files.ssh-private-key.path;
      publicKey = builtins.readFile config.clan.core.vars.generators.radicle.files.ssh-public-key.path;

      node = {
        openFirewall = true;
        listenAddress = "[::]";
        listenPort = 8776;
      };

      settings = {
        preferredSeeds = [
          "z6MkrLMMsiPWUcNPHcRajuMi9mDfYckSoJyPwwnknocNYPm7@seed.radicle.xyz:8776"
          "z6Mkmqogy2qEM2ummccUthFEaaHvyYmYBYh3dbe9W4ebScxo@iris.radicle.xyz:8776"
        ];
        node = {
          alias = config.networking.hostName;
          seedingPolicy = {
            default = "block";
            scope = "all";
          };
          # Auto-follow your DID to accept all your repos
          follow = [
            "did:key:z6MkjE3BSJn4Y129rhqi5rViSUru8KSBcCQdQcDZq1cnjumw"
          ];
          # Peer with other personal nodes
          connect = [
            # eve
            "z6MktZckvzz29eJtUQ4u9bkNu8jihg1sRvknUZMm1xq2stn9@radicle.thalheim.io:8776"
            # eva
            "z6MkwQTGzGVFjmT54Ustr82rc3bMGkjSjeCXQWgSvNNvVnwa@eva.thalheim.io:8776"
            # blob64
            "z6Mkkmnifhqr7bJ48tKjE3KRXKwH9SSwMavNPfsphCpeT94W@blob64.x:8776"
          ];
        };
        # Pin explicitly configured seed repositories in the web interface
        web.pinned.repositories = cfg.seedRepositories;
      };
    };

    # Ensure follow policy and repo seeding is set on startup
    systemd.services.radicle-node-setup = {
      description = "Initialize Radicle node follow policies and seed repositories";
      after = [ "radicle-node.service" ];
      wants = [ "radicle-node.service" ];
      wantedBy = [ "multi-user.target" ];
      environment = {
        HOME = "/var/lib/radicle";
        RAD_HOME = "/var/lib/radicle";
      };
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        User = "radicle";
        Group = "radicle";
        LoadCredential = "radicle:${config.clan.core.vars.generators.radicle.files.ssh-private-key.path}";
        BindReadOnlyPaths = [
          "${config.services.radicle.configFile}:/var/lib/radicle/config.json"
          "/run/credentials/radicle-node-setup.service/radicle:/var/lib/radicle/keys/radicle"
          "${config.clan.core.vars.generators.radicle.files.ssh-public-key.path}:/var/lib/radicle/keys/radicle.pub"
        ];
        StateDirectory = "radicle";
        StateDirectoryMode = "0750";
      };
      path = [ config.services.radicle.package ];
      script = ''
        # Wait for radicle-node to be ready
        for i in {1..30}; do
          if rad node status &>/dev/null; then
            break
          fi
          sleep 1
        done

        # Ensure follow policy is set
        rad follow did:key:z6MkjE3BSJn4Y129rhqi5rViSUru8KSBcCQdQcDZq1cnjumw --alias mic92 || true

        # Seed explicitly configured repositories
        ${lib.concatMapStringsSep "\n" (rid: ''
          rad seed ${lib.escapeShellArg rid} --scope all || true
        '') cfg.seedRepositories}
      '';
    };

    # Periodic discovery and seeding of repos from followed users
    systemd.services.radicle-auto-seed = lib.mkIf cfg.autoSeedFollowed {
      description = "Auto-discover and seed repositories from followed users";
      after = [ "radicle-node.service" ];
      wants = [ "radicle-node.service" ];
      environment = {
        HOME = "/var/lib/radicle";
        RAD_HOME = "/var/lib/radicle";
      };
      serviceConfig = {
        Type = "oneshot";
        User = "radicle";
        Group = "radicle";
        LoadCredential = "radicle:${config.clan.core.vars.generators.radicle.files.ssh-private-key.path}";
        BindReadOnlyPaths = [
          "${config.services.radicle.configFile}:/var/lib/radicle/config.json"
          "/run/credentials/radicle-auto-seed.service/radicle:/var/lib/radicle/keys/radicle"
          "${config.clan.core.vars.generators.radicle.files.ssh-public-key.path}:/var/lib/radicle/keys/radicle.pub"
        ];
        StateDirectory = "radicle";
        StateDirectoryMode = "0750";
      };
      path = [ config.services.radicle.package ];
      script = ''
        # Get all followed NIDs by parsing the table output
        rad follow 2>/dev/null | grep 'did:key:' | sed 's/.*did:key:\([a-zA-Z0-9]*\).*/\1/' | while read -r nid; do
          echo "Discovering repos from followed user $nid..."
          # Get inventory from each followed user
          rad node inventory --nid "$nid" 2>/dev/null | while read -r rid; do
            # Check if already seeded
            if ! rad seed 2>/dev/null | grep -q "$rid"; then
              echo "Auto-seeding discovered repo: $rid"
              rad seed "$rid" --scope all || true
            fi
          done
        done
      '';
    };

    systemd.timers.radicle-auto-seed = lib.mkIf cfg.autoSeedFollowed {
      description = "Timer for auto-seeding repos from followed users";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnBootSec = "5min";
        OnUnitActiveSec = "5h";
        RandomizedDelaySec = "10min";
      };
    };
  };
}
