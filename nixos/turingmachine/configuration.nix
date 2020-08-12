{ config, pkgs, lib, modulesPath, ... }:
{
  imports = [
    ./hardware-configuration.nix

    #../modules/libvirt.nix
    ./modules/caddy.nix
    ./modules/dice.nix
    ./modules/backup.nix
    ./modules/light
    #./modules/high-dpi.nix
    ./modules/nfs.nix
    ./modules/remote-builder.nix
    ./modules/retiolum.nix
    ./modules/ssmtp.nix
    #../modules/sway.nix
    ./modules/minidlna.nix
    ./modules/networkmanager.nix
    #./modules/nfs-dl.nix
    ./modules/packages.nix
    ./modules/high-dpi.nix
    ./modules/gnome-pim.nix
    ./modules/rhasspy.nix
    ./modules/sops.nix
    ./modules/sshd.nix

    ../modules/macos-kvm.nix
    ../modules/mosh.nix
    ../modules/networkd.nix
    ../modules/dnsmasq.nix
    ../modules/wireguard.nix
    ../modules/tracing.nix
    #./kde.nix
    ../modules/i3.nix
    #../modules/sway.nix
    #../modules/awesome.nix
    #../modules/gnome.nix
    ../modules/pki
    ../modules/yubikey.nix
    ../modules/zfs.nix
    ../modules/users.nix
  ];

  #programs.captive-browser.enable = true;
  #programs.captive-browser.interface = "wlan0";

  boot = {
    loader.systemd-boot.enable = true;
    # when installing toggle this
    loader.efi.canTouchEfiVariables = false;

    # It may leak your data, but look how FAST it is!1!!
    # https://make-linux-fast-again.com/
    kernelParams = [
      "noibrs"
      "noibpb"
      "nopti"
      "nospectre_v2"
      "nospectre_v1"
      "l1tf=off"
      "nospec_store_bypass_disable"
      "no_stf_barrier"
      "mds=off"
      "mitigations=off"
    ];
  };

  networking.hostName = "turingmachine";

  nix.binaryCaches = [
    "https://r-ryantm.cachix.org"
  ];
  nix.binaryCachePublicKeys = [
    "r-ryantm.cachix.org-1:gkUbLkouDAyvBdpBX0JOdIiD2/DP1ldF3Z3Y6Gqcc4c="
  ];

  console.keyMap = "us";
  i18n.defaultLocale = "en_DK.UTF-8";

  # Manual timezones, also see modules/networkmanager.py
  time.timeZone = null;

  services = {
    gpm.enable = true;
    upower.enable = true;
    locate.enable = true;
    openssh = {
      enable = true;
      forwardX11 = true;
    };

    avahi.enable = true;
    avahi.nssmdns = true;

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

  powerManagement.powertop.enable = true;

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
    virtualbox.host.enable = false;
    docker = {
      enable = true;
      enableOnBoot = true;
      storageDriver = "zfs";
      extraOptions = "--userland-proxy=false --ip-masq=true --storage-opt=zfs.fsname=zroot/docker --dns=8.8.8.8";
    };
  };

  fonts.enableFontDir = true;

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      enableExtraSocket = true;
      pinentryFlavor = "gnome3";
    };

    ssh.extraConfig = ''
      SendEnv LANG LC_*
    '';
    adb.enable = true;
    bash.enableCompletion = true;
    zsh = {
      enable = true;
      promptInit = "";
    };
  };

  hardware.pulseaudio.enable = true;

  security.audit.enable = false;

  #services.teamviewer.enable = true;

  nixpkgs.config = {
    allowUnfree = true;
    android_sdk.accept_license = true;
  };

  services.tor.client.enable = true;

  #services.pixiecore = let
  #  nixos = import "${modulesPath}/.." {
  #    inherit (pkgs) system;
  #    configuration = { config, pkgs, lib, ... }: with lib; {
  #      imports = [ "${modulesPath}/installer/netboot/netboot-minimal.nix" ];
  #      # Some useful options for setting up a new system
  #      services.mingetty.autologinUser = mkForce "root";
  #      # Enable sshd which gets disabled by netboot-minimal.nix
  #      systemd.services.sshd.wantedBy = mkOverride 0 [ "multi-user.target" ];
  #      # users.users.root.openssh.authorizedKeys.keys = [ ... ];
  #      # i18n.consoleKeyMap = "de";
  #    };
  #  };
  #  build = nixos.config.system.build;
  #in {
  #  enable = true;
  #  openFirewall = true;
  #  mode = "boot";
  #  kernel = "${build.kernel}/bzImage";
  #  initrd = "${toString build.netbootRamdisk}/initrd";
  #  cmdLine = "init=${build.netbootIpxeScript} ${lib.concatStringsSep " " nixos.config.boot.kernelParams} debug";
  #  dhcpNoBind = true;
  #};

  services.samba = {
    enable = true;
    securityType = "user";
    extraConfig = ''
      workgroup = WORKGROUP
      server string = smbnix
      netbios name = smbnix
      security = user
      hosts allow = 0.0.0.0/0
      guest account = nobody
      map to guest = bad user
    '';
    shares = {
      public = {
        path = "/home/joerg/web/upload";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "yes";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "joerg";
        "force group" = "users";
      };
    };
  };
  networking.firewall.interfaces."virbr1".allowedTCPPorts = [
    445 139
  ];
  networking.firewall.interfaces."virbr1".allowedUDPPorts = [
    445 139
  ];

  documentation.doc.enable = false;

  system.stateVersion = "18.03";

  systemd.services.rhasspy = {
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    # rhasspy sets `/dev/stdout` as log file for supervisord
    # supervisord tries to open /dev/stdout and fails with the default systemd device
    # it works for pipes so...
    script = ''
      ${pkgs.nur.repos.mic92.rhasspy}/bin/rhasspy --profile en | ${pkgs.utillinux}/bin/logger
    '';
    serviceConfig = {
      User = "joerg";
      # needed for pulseaudio
      Environment = "XDG_RUNTIME_DIR=/run/user/1000";
    };
  };
}
