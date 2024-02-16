{ pkgs
, lib
, config
, ...
}: {
  imports = [
    ./hardware-configuration.nix

    ./modules/caddy.nix
    ./modules/disko.nix
    ./modules/hass-agent.nix
    ./modules/networkmanager
    ./modules/nfs.nix
    ./modules/packages.nix
    ./modules/postgresql.nix
    ./modules/tum-vpn.nix
    ./modules/toggle-keyboard

    ../modules/borgbackup.nix
    ../modules/i18n.nix
    ../modules/ip-update.nix
    ../modules/kde
    #../modules/qtile.nix
    ../modules/keyd.nix
    ../modules/ksmbd.nix
    ../modules/lanzaboote.nix
    ../modules/make-linux-fast.nix
    ../modules/mosh.nix
    ../modules/networkd.nix
    ../modules/nix-ld.nix
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
    ../modules/users.nix
  ];

  services.udev.packages = with pkgs; [ platformio-core.udev ];

  services.pcscd.enable = true;

  systemd.sysusers.enable = true;
  users.mutableUsers = false;
  users.users.joerg.initialHashedPassword = config.clanCore.secrets.root-password.facts.root-password-hash.value;

  # https://community.frame.work/t/guide-linux-battery-life-tuning/6665
  #services.tlp.enable = true;
  #services.tlp.settings."PCIE_ASPM_ON_BAT" = "powersupersave";

  hardware.keyboard.qmk.enable = true;

  #services.udev.packages = [ pkgs.platformio ];

  services.gvfs.enable = true;

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
    bash.enableCompletion = true;
    zsh = {
      enable = true;
      promptInit = "";
    };
  };

  security.audit.enable = false;

  services.tor.client.enable = true;

  #services.samba = {
  #  enable = true;
  #  securityType = "user";
  #  enableWinbindd = false;
  #  extraConfig = ''
  #    workgroup = WORKGROUP
  #    server string = smbnix
  #    netbios name = smbnix
  #    security = user
  #    hosts allow = 0.0.0.0/0
  #    guest account = nobody
  #    map to guest = bad user
  #    # Use sendfile() for performance gain
  #    use sendfile = true

  #    # No NetBIOS is needed
  #    disable netbios = true

  #    # Only mangle non-valid NTFS names, don't care about DOS support
  #    mangled names = illegal

  #    # Performance optimizations
  #    socket options = TCP_NODELAY IPTOS_LOWDELAY SO_RCVBUF=65536 SO_SNDBUF=65536

  #    # Disable all printing
  #    load printers = false
  #    disable spoolss = true
  #    printcap name = /dev/null
  #  '';
  #  shares = {
  #    public = {
  #      path = "/home/joerg/web/upload";
  #      browseable = "yes";
  #      "read only" = "no";
  #      "guest ok" = "yes";
  #      "create mask" = "0644";
  #      "directory mask" = "0755";
  #      "force user" = "joerg";
  #      "force group" = "users";
  #    };
  #  };
  #};

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

  security.sudo.wheelNeedsPassword = lib.mkForce true; # fprint

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
