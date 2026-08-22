{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.rustdesk-client;
  configFile = pkgs.writeText "RustDesk2.toml" ''
    rendezvous_server = '${cfg.server}:21116'
    nat_type = 1
    serial = 0

    [options]
    key = '${cfg.key}'
    custom-rendezvous-server = '${cfg.server}'
    relay-server = '${cfg.server}'
  '';
in
{
  options.services.rustdesk-client = {
    server = lib.mkOption {
      type = lib.types.str;
      default = "rustdesk.thalheim.io";
      description = "RustDesk server hostname";
    };

    key = lib.mkOption {
      type = lib.types.str;
      default = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPYd1Vk9KbTEEvQtxwhZYoPwZl/kS1LMjyg5AjL/0NKz rustdesk";
      description = "RustDesk server public key";
    };

    users = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "List of users to configure RustDesk for";
    };

    package = lib.mkPackageOption pkgs "rustdesk-flutter" { };
  };

  config = {
    environment.systemPackages = [ cfg.package ];

    # Background service for remote input injection (mouse/keyboard)
    systemd.services.rustdesk = {
      description = "RustDesk";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = "${cfg.package}/bin/rustdesk --service";
        Restart = "always";
        Type = "simple";
      };
    };

    # Required for input injection on Wayland
    boot.kernelModules = [ "uinput" ];

    services.udev.extraRules = ''
      KERNEL=="uinput", MODE="0660", GROUP="input"
    '';

    systemd.tmpfiles.rules = [
      # The background service runs as root; without this it falls back to
      # the public rendezvous servers and keeps syncing that back to user
      # configs via IPC.
      "d /root/.config/rustdesk 0700 root root -"
      "C+ /root/.config/rustdesk/RustDesk2.toml 0600 root root - ${configFile}"
    ]
    ++ lib.flatten (
      map (user: [
        "d /home/${user}/.config/rustdesk 0755 ${user} users -"
        "C+ /home/${user}/.config/rustdesk/RustDesk2.toml 0644 ${user} users - ${configFile}"
        # Desktop shortcut following the current system closure (never stale).
        # Both English and German desktop dir names; ensure they are
        # user-owned so L+ does not create them as root.
        "d /home/${user}/Desktop 0755 ${user} users -"
        "d /home/${user}/Schreibtisch 0755 ${user} users -"
        "L+ /home/${user}/Desktop/rustdesk.desktop - - - - /run/current-system/sw/share/applications/rustdesk.desktop"
        "L+ /home/${user}/Schreibtisch/rustdesk.desktop - - - - /run/current-system/sw/share/applications/rustdesk.desktop"
      ]) cfg.users
    );
  };
}
