{
  pkgs,
  lib,
  config,
  inputs,
  self,
  ...
}:
{
  imports = [
    self.nixosModules.default
    inputs.nixos-hardware.nixosModules.framework-13th-gen-intel
    inputs.nix-index-database.nixosModules.nix-index
    { programs.nix-index-database.comma.enable = true; }
    inputs.disko.nixosModules.disko
    inputs.clan-core.clanModules.localbackup

    #inputs.spora.nixosModules.spora

    inputs.srvos.nixosModules.desktop

    inputs.lanzaboote.nixosModules.lanzaboote

    ./modules/caddy.nix
    ./modules/disko.nix
    ./modules/hass-agent.nix
    ./modules/networkmanager
    ./modules/nfs.nix
    ./modules/packages.nix
    ./modules/postgresql.nix
    ./modules/tum-vpn.nix
    ./modules/toggle-keyboard
    ./modules/localbackup.nix
    ./modules/sunshine.nix
    ./modules/ghaf-hardware.nix
    ./modules/ghaf-facter.nix

    ../modules/i18n.nix
    ../modules/ip-update.nix
    ../modules/kde
    #../modules/qtile.nix
    #../modules/keyd.nix
    ../modules/ksmbd.nix
    ../modules/lanzaboote.nix
    ../modules/make-linux-fast.nix
    ../modules/mosh.nix
    ../modules/networkd.nix
    ../modules/nncp.nix
    ../modules/no-hz.nix
    ../modules/pipewire.nix
    ../modules/powertop.nix
    ../modules/promtail.nix
    ../modules/remote-builder.nix
    ../modules/sshd/tor.nix
    ../modules/suspend-on-low-power.nix
    ../modules/touchpad-hack
    ../modules/tracing.nix
    ../modules/data-mesher.nix
    ../modules/users.nix
  ];

  hardware.graphics.enable32Bit = config.hardware.graphics.enable;

  nixpkgs.pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;

  system.etc.overlay.enable = true;
  system.etc.overlay.mutable = true;
  services.userborn.enable = true;

  environment.systemPackages = with pkgs; [
    sunshine
    nixos-facter
  ];

  services.fwupd.enable = true;

  hardware.saleae-logic.enable = true;

  services.ollama.enable = true;

  services.udev.packages = with pkgs; [ platformio-core.udev ];

  services.pcscd.enable = true;

  users.mutableUsers = false;
  users.users.joerg.hashedPasswordFile =
    config.clan.core.facts.services.root-password.secret.password-hash.path;

  # https://community.frame.work/t/guide-linux-battery-life-tuning/6665
  #services.tlp.enable = true;
  #services.tlp.settings."PCIE_ASPM_ON_BAT" = "powersupersave";

  hardware.keyboard.qmk.enable = true;

  #services.udev.packages = [ pkgs.platformio ];

  services.gvfs.enable = true;

  #environment.sessionVariables.KWIN_DRM_ALLOW_INTEL_COLORSPACE = "1";
  #services.displayManager.environment.KWIN_DRM_ALLOW_INTEL_COLORSPACE = "1";

  boot.plymouth.enable = true;

  networking.hostName = "turingmachine";

  console.keyMap = "us";

  # Manual timezones, also see modules/networkmanager.py
  time.timeZone = null;

  programs.wireshark.enable = true;

  services = {
    gpm.enable = true;
    upower.enable = true;

    printing = {
      enable = true;
      browsing = true;
      drivers = [ pkgs.gutenprint ]; # pkgs.hplip
    };

    logind.extraConfig = ''
      LidSwitchIgnoreInhibited=no
      HandlePowerKey=ignore
    '';
    journald.extraConfig = "SystemMaxUse=1G";
  };

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

  virtualisation = {
    #anbox.enable = true;
    #lxc.enable = true;
    #lxd.enable = true;
    #rkt.enable = true;
    #rkt.enable = true;
    virtualbox.host.enable = false;
    docker.enable = true;
    docker.storageDriver = "zfs";
    docker.extraOptions = "--storage-opt=zfs.fsname=zroot/docker";
  };

  fonts.fontDir.enable = true;

  environment.variables.SSH_ASKPASS = lib.mkForce "${pkgs.lxqt.lxqt-openssh-askpass}/bin/lxqt-openssh-askpass";
  programs = {
    ssh = {
      startAgent = true;
      askPassword = "${pkgs.lxqt.lxqt-openssh-askpass}/bin/lxqt-openssh-askpass";
      extraConfig = ''
        SendEnv LANG LC_*
      '';
    };
    adb.enable = true;
    zsh = {
      enable = true;
      promptInit = "";
    };
  };

  system.switch.enableNg = true;
  system.switch.enable = false;

  security.audit.enable = false;

  services.tor.client.enable = true;

  networking.firewall.allowedTCPPorts = [ 8081 ];

  boot.binfmt.emulatedSystems = [
    "armv7l-linux"
    "aarch64-linux"
    "riscv32-linux"
    "riscv64-linux"
    "powerpc64-linux"
    "powerpc64le-linux"
  ];

  system.stateVersion = "23.11";
  boot.initrd.systemd.enable = true;

  security.sudo.wheelNeedsPassword = lib.mkForce (!config.services.fprintd.enable); # fprint

  services.ksmbd.enable = true;
  services.ksmbd.openFirewall = true;
  services.ksmbd.shares.public = {
    path = "/var/lib/ksmbd";
    "read only" = true;
    browseable = "yes";
    "guest ok" = "yes";
    comment = "Public samba share.";
  };
}
