{ config, pkgs, lib, ... }:
{
  imports = [
    ./hardware-configuration.nix

    #../modules/libvirt.nix
    ./modules/caddy.nix
    ./modules/dice.nix
    ./modules/backup.nix
    ./modules/high-dpi.nix
    ./modules/nfs.nix
    ./modules/remote-builder.nix
    ./modules/retiolum.nix
    ./modules/ssmtp.nix
    ./modules/stubby.nix
    #../modules/sway.nix
    ./modules/minidlna.nix
    ./modules/networkmanager.nix
    ./modules/packages.nix

    ../modules/mosh.nix
    ../modules/nix-daemon.nix
    ../modules/networkd.nix
    #./vm/modules/dnsmasq.nix
    ../modules/wireguard.nix
    ../modules/secrets.nix
    ../modules/tracing.nix
    ../modules/tor-ssh.nix
    #./kde.nix
    ../modules/i3.nix
    #../modules/awesome.nix
    ../modules/pki
    ../modules/yubikey.nix
    ../modules/zfs.nix
  ];

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

  nix = {
    binaryCaches = [ https://r-ryantm.cachix.org ];
    binaryCachePublicKeys = [ "r-ryantm.cachix.org-1:gkUbLkouDAyvBdpBX0JOdIiD2/DP1ldF3Z3Y6Gqcc4c=" ];
    nixPath = [
      "/var/src"
      "/nix/var/nix/profiles/per-user/root/channels"
    ];
    extraOptions = ''
     builders-use-substitutes = true
   '';
  };

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

    samba = {
      enable = false;
      enableWinbindd = false;
    };

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
      extraOptions = "--userland-proxy=false --ip-masq=true --storage-opt=zfs.fsname=zroot/docker";
    };
  };

  fonts.enableFontDir = true;

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      enableExtraSocket = true;
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

  users.users.joerg = {
    isNormalUser = true;
    extraGroups = [
      "wheel" "docker" "plugdev" "vboxusers" "adbusers" "input" "wireshark"
    ];
    shell = "/run/current-system/sw/bin/zsh";
    uid = 1000;
  };
  users.groups.adbusers = {};
  security.sudo.wheelNeedsPassword = false;

  security.audit.enable = false;

  services.dbus.packages = with pkgs; [ gnome3.dconf ];
  #services.teamviewer.enable = true;

  nixpkgs.config = {
    allowUnfree = true;
    android_sdk.accept_license = true;
  };

  networking.nameservers = [ "1.1.1.1" ];
  services.resolved.enable = false;

  services.tor.client.enable = true;

  system.stateVersion = "18.03";
}
