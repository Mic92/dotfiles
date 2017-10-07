# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

let

  userservice = args: {
    name = args.name;
    value = {
      wantedBy = [ "default.target" ];
      enable = true;
      serviceConfig = {
        RestartSec="500ms";
        ExecStart="/run/current-system/sw/bin/sh -c '" +
          "export GTK_DATA_PREFIX=/run/current-system/sw;" +
          "export PATH=$PATH:/var/setuid-wrappers:%h/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/run/current-system/sw/bin:/run/current-system/sw/sbin;" +
          "exec ${args.command}'";
      };
    };
  };

in {
  imports = [ # Include the results of the hardware scan.or
    ./hardware-configuration.nix
    ./network-configuration.nix
    ./bird.nix
    ./packages.nix
    ./hardware/x250.nix
    ./nspawn-container
    ./dice.nix
    ./backup.nix
    ./nfs.nix
    ./vms/modules/mosh.nix
    ./vms/modules/overlay.nix
    ./vms/modules/tracing.nix
  ];

  boot = {
    plymouth.enable = true;
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    blacklistedKernelModules = [ "iptable_nat" "ip_tables" ];
  };

  environment.sessionVariables = {
    # so gtk2.0/gtk3.0 themes can be found
    GTK_DATA_PREFIX = "/run/current-system/sw";
    LD_LIBRARY_PATH = [ config.system.nssModules.path ];
  };

  nix = {
    gc.automatic = true;
    gc.dates = "03:15";
    #binaryCaches = [ https://cache.nixos.community https://cache.nixos.org/ ];
    distributedBuilds = false;
    #buildMachines = [
    #  { hostName = "inspector.r"; sshUser = "nix"; sshKey = "/etc/nixos/secrets/id_buildfarm"; system = "x86_64-linux"; maxJobs = 8; }
    #];
    package = pkgs.nixUnstable;
    nixPath = [
      "nixpkgs=/home/joerg/git/nixpkgs"
      "nixos-config=/home/joerg/git/nixos-configuration/configuration.nix"
      "nspawn-container=/home/joerg/git/nixos-configuration/nspawn-container"
      "nixpkgs-overlays=/home/joerg/git/nixos-configuration/overlays"
    ];
    nrBuildUsers = 30;
    useSandbox = true;
    extraOptions = ''
      gc-keep-outputs = true
      gc-keep-derivations = true
      build-max-jobs = 10
    '';
  };

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_DK.UTF-8";
  };
  time.timeZone = "Europe/London";

  boot.extraModprobeConfig = ''
    options zfs zfs_arc_max=34359738368
  '';
  #boot.zfs.enableUnstable = true;

  services = {
    physlock.enable = true;
    gnome3.gnome-keyring.enable = true;
    autorandr.enable = true;
    resilio = {
      enable = true;
      enableWebUI = true;
    };
    upower.enable = true;
    locate.enable = true;
    tor = {
      enable = true;
      hiddenServices."turingmachine".map = [
        { port = 22; }
        { port = 2015; }
      ];
      extraConfig = ''
        DNSPort 9053
        AutomapHostsOnResolve 1
        AutomapHostsSuffixes .exit,.onion
        EnforceDistinctSubnets 1
        ExitNodes {de}
        EntryNodes {de}
        NewCircuitPeriod 120
      '';
    };
    polipo = {
      enable = true;
      socksParentProxy = "localhost:9050";
    };
    nscd.enable = true;
    zfs = {
      autoSnapshot.enable = true;
      autoScrub.enable = true;
    };

    openssh = {
      enable = true;
      forwardX11 = true;
    };

    xserver = {
      enable = true;
      layout = "us";
      xkbVariant = "altgr-intl";
      xkbOptions = "caps:ctrl_modifier,compose:menu";
      windowManager = {
        awesome = {
          enable = true;
          luaModules = with pkgs.lua52Packages; [ luasocket ];
        };
        default = "awesome";
      };
    };

    avahi.enable = true;

    samba = {
      enable = true;
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

  #systemd.package = pkgs.mysystemd;

  powerManagement.powertop.enable = true;

  systemd.services = {
    systemd-networkd-wait-online.enable = false;

    caddy = let
      cfg = pkgs.writeText "Caddyfile" ''
        0.0.0.0 {
          timeouts 0
          tls off
          #markdown
          browse
          root /home/joerg/web

          basicauth /privat root cakeistasty
          basicauth /private root kuchenistlecker
        }
      '';
    in {
      description = "Caddy web server";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = ''${pkgs.caddy.bin}/bin/caddy -conf=${cfg} -agree'';
        User = "joerg";
        AmbientCapabilities = "cap_net_bind_service";
      };
    };

    audio-off = {
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

    wifi-scan = {
      description = "Scan wlan after suspend";
      wantedBy = [ "sleep.target" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = "yes";
        ExecStart = "${pkgs.coreutils}/bin/true";
        ExecStop = "${pkgs.wpa_supplicant}/bin/wpa_cli scan";
      };
    };
    btsync = with pkgs; {
      wantedBy = [ "multi-user.target" ];
      description = "Bittorrent Sync user service";
      after       = [ "network.target" "local-fs.target" ];
      serviceConfig = {
        Restart   = "on-abort";
        User = "joerg";
        ExecStart = "${pkgs.bittorrentSync20}/bin/btsync --nodaemon --config /home/joerg/.config/btsync/btsync.conf";
      };
    };
    systemd-udev-settle.serviceConfig.ExecStart = "${pkgs.coreutils}/bin/true";
  };

  virtualisation = {
    virtualbox.host.enable = true;
    docker = {
      enable = true;
      enableOnBoot = true;
      storageDriver = "zfs";
      extraOptions = "--iptables=false --storage-opt=zfs.fsname=zroot/docker --userns-remap=docker";
    };
  };

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
  };

  programs = {
    ssh.startAgent = true;
    light.enable = true;
    adb.enable = true;
    zsh = {
      enable = true;
      promptInit = "";
    };
  };

  hardware.pulseaudio.enable = true;

  users.extraUsers = {
    joerg = {
      isNormalUser = true;
      extraGroups = ["wheel" "docker" "plugdev" "vboxusers" "adbusers" "input"];
      shell = "/run/current-system/sw/bin/zsh";
      uid = 1000;
    };
    docker = {
      subUidRanges = [ { startUid = 100000; count = 65536; } ];
      subGidRanges = [ { startGid = 100000; count = 65536; } ];
    };
  };
  users.extraGroups.adbusers = {};

  security = {
    audit.enable = false;
    #apparmor.enable = true;
    sudo.wheelNeedsPassword = false;
  };

  services.dbus.packages = with pkgs; [ gnome3.dconf ];

  system.stateVersion = "17.03";

  #containers.database = {
  #  config = { config, pkgs, ... }: {
  #    services.postgresql.enable = true;
  #    environment.systemPackages = [ pkgs.htop ];
  #  };
  #};
}
