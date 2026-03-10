# iroh-ssh NixOS module — persistent SSH-over-iroh server
#
# Runs iroh-ssh as a systemd service with DynamicUser.  Keys are
# generated once via clan vars and placed into the service's state
# directory so the endpoint ID stays stable across restarts.
{
  config,
  lib,
  pkgs,
  self,
  ...
}:
let
  cfg = config.services.iroh-ssh;
  iroh-ssh = self.packages.${pkgs.stdenv.hostPlatform.system}.iroh-ssh;
in
{
  options.services.iroh-ssh = {
    enable = lib.mkEnableOption "iroh-ssh server";

    sshPort = lib.mkOption {
      type = lib.types.port;
      default = 22;
      description = "Local SSH port iroh-ssh forwards incoming connections to.";
    };
  };

  config = lib.mkIf cfg.enable {
    # Make the CLI available system-wide (for `iroh-ssh proxy`, `iroh-ssh info`, etc.)
    environment.systemPackages = [ iroh-ssh ];

    # --- key generation via clan vars ---
    clan.core.vars.generators.iroh-ssh = {
      files."irohssh_ed25519" = { };
      files."irohssh_ed25519.pub".secret = false;
      runtimeInputs = [
        (pkgs.python3.withPackages (ps: [ ps.cryptography ]))
      ];
      script = ''
        python3 -c "
        import base64, os
        from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey
        from cryptography.hazmat.primitives import serialization

        Z32 = 'ybndrfg8ejkmcpqxot1uwisza345h769'
        STD = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567'

        def z32_encode(data):
            b32 = base64.b32encode(data).decode().rstrip('=')
            return b32.translate(str.maketrans(STD + STD.lower(), Z32 + Z32))

        key = Ed25519PrivateKey.generate()
        priv_bytes = key.private_bytes(
            serialization.Encoding.Raw,
            serialization.PrivateFormat.Raw,
            serialization.NoEncryption(),
        )
        pub_bytes = key.public_key().public_bytes(
            serialization.Encoding.Raw,
            serialization.PublicFormat.Raw,
        )

        with open(os.environ['out'] + '/irohssh_ed25519', 'w') as f:
            f.write(z32_encode(priv_bytes))
        with open(os.environ['out'] + '/irohssh_ed25519.pub', 'w') as f:
            f.write(z32_encode(pub_bytes))
        "
      '';
    };

    # --- systemd service ---
    systemd.services.iroh-ssh = {
      description = "iroh-ssh server";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "simple";
        DynamicUser = true;
        StateDirectory = "iroh-ssh";
        ExecStartPre = pkgs.writeShellScript "iroh-ssh-setup-keys" ''
          # Populate $HOME/.ssh from vars-managed keys so iroh-ssh
          # finds its persistent identity in the usual location.
          mkdir -p "$STATE_DIRECTORY/.ssh"
          install -m 0600 ${config.clan.core.vars.generators.iroh-ssh.files."irohssh_ed25519".path} \
            "$STATE_DIRECTORY/.ssh/irohssh_ed25519"
          install -m 0644 ${config.clan.core.vars.generators.iroh-ssh.files."irohssh_ed25519.pub".path} \
            "$STATE_DIRECTORY/.ssh/irohssh_ed25519.pub"
        '';
        ExecStart = "${iroh-ssh}/bin/iroh-ssh server --persist --ssh-port ${toString cfg.sshPort}";
        Environment = "HOME=/var/lib/iroh-ssh";
        Restart = "on-failure";
        RestartSec = 5;

        # Hardening
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        NoNewPrivileges = true;
      };
    };
  };
}
