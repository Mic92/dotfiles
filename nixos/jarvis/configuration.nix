{
  config,
  pkgs,
  lib,
  modulesPath,
  ...
}: {
  imports = [
    ./hardware-configuration.nix

    #./modules/rhasspy
    ./modules/sshd.nix
    ./modules/light
    ./modules/network.nix
    ./modules/vlc-telnet.nix

    ../modules/hass-agent.nix
    ../modules/ip-update.nix
    ../modules/make-linux-fast.nix
    ../modules/promtail.nix
    ../modules/powertop.nix
    ../modules/packages.nix
    ../modules/mosh.nix
    ../modules/tracing.nix
    ../modules/telegraf.nix
    ../modules/pki
    ../modules/xrdp.nix
    #../modules/xrdp.nix
    ../modules/zfs.nix
    ../modules/users.nix
  ];

  boot = {
    zfs.requestEncryptionCredentials = [
      "zroot/root"
    ];

    loader.systemd-boot.enable = true;
    # when installing toggle this
    loader.efi.canTouchEfiVariables = false;
  };

  networking.hostName = "jarvis";

  sops.defaultSopsFile = ./secrets/secrets.yaml;

  console.keyMap = "us";

  i18n.defaultLocale = "en_DK.UTF-8";

  time.timeZone = "utc";

  services = {
    gpm.enable = true;
    upower.enable = true;

    logind.lidSwitch = "ignore";
    journald.extraConfig = "SystemMaxUse=1G";
  };

  documentation.enable = false;

  system.stateVersion = "18.03";

  sops.secrets.xrdp-password.neededForUsers = true;
  users.users.joerg.passwordFile = config.sops.secrets.xrdp-password.path;
}
