{
  pkgs,
  lib,
  config,
  ...
}:
{
  # Common desktop/laptop workstation configuration

  imports = [
    ./docker-zfs.nix
    ./i18n.nix
    ./ip-update.nix
    ./kde
    ./limine.nix
    ./networkd.nix
    ./packages.nix
    ./pipewire.nix
    ./powertop.nix
    ./promtail.nix
    ./remote-builder.nix
    ./tracing.nix
    ./users.nix
    ./hyprspace.nix
  ];

  # System configuration
  system.etc.overlay.enable = true;
  system.etc.overlay.mutable = true;
  services.userborn.enable = true;

  # Hardware support
  services.fwupd.enable = true;
  hardware.keyboard.qmk.enable = true;

  # Desktop services
  services.gvfs.enable = true;

  # Boot configuration
  boot.plymouth.enable = true;
  boot.initrd.systemd.enable = true;

  # ZFS performance tuning
  boot.kernelParams = [
    # defaults is 5, but this can be quite slow for sqlite databases
    "zfs.zfs_txg_timeout=2"
  ];

  # Console and locale
  console.keyMap = "us";

  # Manual timezones (set by networkmanager dispatcher or manually)
  time.timeZone = null;

  # Development tools
  programs.wireshark.enable = true;
  programs.adb.enable = true;

  # Services
  services = {
    gpm.enable = true;
    upower.enable = true;

    printing = {
      enable = true;
      browsing = true;
      drivers = [ pkgs.gutenprint ];
    };

    journald.extraConfig = "SystemMaxUse=1G";

    zerotierone.joinNetworks = [ "a9b4872919354736" ];
    tor.client.enable = true;
  };

  # Mute audio before suspend
  systemd.services.audio-off = {
    description = "Mute audio before suspend";
    wantedBy = [ "sleep.target" ];
    serviceConfig = {
      Type = "oneshot";
      Environment = "XDG_RUNTIME_DIR=/run/user/1000";
      User = "joerg";
      RemainAfterExit = "yes";
      ExecStart = "${pkgs.pamixer}/bin/pamixer --mute";
    };
  };

  # Fonts
  fonts.fontDir.enable = true;

  # Programs
  programs = {
    ssh = {
      askPassword = "${pkgs.lxqt.lxqt-openssh-askpass}/bin/lxqt-openssh-askpass";
      extraConfig = ''
        SendEnv LANG LC_*
      '';
    };
    zsh = {
      enable = true;
      promptInit = "";
    };
  };

  # Security
  security.audit.enable = false;
  security.sudo.wheelNeedsPassword = lib.mkForce (!config.services.fprintd.enable);

  # Cross-architecture support
  boot.binfmt.emulatedSystems = [
    "armv7l-linux"
    "riscv32-linux"
    "riscv64-linux"
    "powerpc64-linux"
    "powerpc64le-linux"
  ];

  # User management
  users.mutableUsers = false;
  users.users.joerg.hashedPasswordFile =
    config.clan.core.vars.generators.user-password-root.files.user-password-hash.path;

  # Network
  networking.networkmanager.enable = lib.mkDefault true;
}
