{ config, pkgs, lib, modulesPath, ... }:
{
  imports = [
    ./hardware-configuration.nix

    #../modules/libvirt.nix
    ./modules/caddy.nix
    ./modules/dice.nix
    ./modules/backup.nix
    ./modules/hass-agent.nix
    ./modules/light
    #./modules/high-dpi.nix
    ./modules/nfs.nix
    ./modules/remote-builder.nix
    ./modules/retiolum.nix
    ./modules/ssmtp.nix
    #../modules/sway.nix
    #./modules/minidlna.nix
    ./modules/networkmanager.nix
    #./modules/nfs-dl.nix
    ./modules/packages.nix
    ./modules/high-dpi.nix
    ./modules/gnome-pim.nix
    ./modules/rhasspy
    ./modules/sops.nix
    ./modules/sshd.nix
    ./modules/cntr.nix
    ./modules/telegraf.nix

    ../modules/promtail.nix
    ../modules/powertop.nix
    ../modules/macos-kvm.nix
    ../modules/mosh.nix
    ../modules/networkd.nix
    ../modules/dnsmasq.nix
    ../modules/wireguard.nix
    ../modules/tracing.nix
    ../modules/telegraf.nix
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

  # required for gpg-agent?
  services.dbus.packages = [ pkgs.gcr ];
  services.udev.packages = [ pkgs.platformio ];

  boot = {
    zfs.requestEncryptionCredentials = [ "zroot/root" ];

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
      "tsx=on"
      "tsx_async_abort=off"
      "mitigations=off"
    ];
  };

  networking.hostName = "turingmachine";

  console.keyMap = "us";
  # Capslock -> Enter
  console.keyMapOverrides = ''
    keycode 58 = Return
  '';

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
    podman.enable = true;
  };

  fonts.fontDir.enable = true;

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      enableExtraSocket = true;
      pinentryFlavor = "qt";
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
    enableWinbindd = false;
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
    445
    139
  ];
  networking.firewall.interfaces."virbr1".allowedUDPPorts = [
    445
    139
  ];

  documentation.doc.enable = false;

  system.stateVersion = "18.03";
}
