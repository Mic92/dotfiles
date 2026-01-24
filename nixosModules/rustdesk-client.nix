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

    systemd.tmpfiles.rules = lib.flatten (
      map (user: [
        "d /home/${user}/.config/rustdesk 0755 ${user} users -"
        "C+ /home/${user}/.config/rustdesk/RustDesk2.toml 0644 ${user} users - ${configFile}"
      ]) cfg.users
    );
  };
}
