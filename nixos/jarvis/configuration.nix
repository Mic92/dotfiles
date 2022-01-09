{ config, pkgs, lib, modulesPath, ... }:
{
  imports = [
    ./hardware-configuration.nix

    ./modules/rhasspy
    ./modules/sshd.nix
    ./modules/network.nix

    ../modules/make-linux-fast.nix
    ../modules/promtail.nix
    ../modules/powertop.nix
    ../modules/packages.nix
    ../modules/mosh.nix
    ../modules/tracing.nix
    ../modules/telegraf.nix
    ../modules/pki
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
  # Capslock -> Enter
  console.keyMapOverrides = ''
    keycode 58 = Return
  '';

  i18n.defaultLocale = "en_DK.UTF-8";

  time.timeZone = "utc";

  services = {
    gpm.enable = true;
    upower.enable = true;
    locate.enable = true;

    logind.lidSwitch = "ignore";
    journald.extraConfig = "SystemMaxUse=1G";
  };

  documentation.enable = false;

  system.stateVersion = "18.03";
}
