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
    #./libvirt.nix
    (builtins.fetchGit {
      url = "https://github.com/NixOS/nixos-hardware";
      rev = "a5fa2cc1ae6a1002962cf71fc23fbd533db412be";
    } + "/lenovo/thinkpad/x250") 
    ./nspawn-container
    ./dice.nix
    ./backup.nix
    ./nfs.nix
    ./vms/modules/zfs.nix
    ./vms/modules/mosh.nix
    ./vms/modules/overlay.nix
    ./vms/modules/tracing.nix
  ];

  boot = {
    #kernelParams = [ "security=selinux" "selinux"];
    #kernelParams = [ "apparmor=1" "security=apparmor" ];
    loader = {
      systemd-boot.enable = true;
      # when installing toggle this
      # efi.canTouchEfiVariables = true;
    };
    blacklistedKernelModules = [ "iptable_nat" "ip_tables" ];
  };

  environment.sessionVariables = {
    # so gtk2.0/gtk3.0 themes can be found
    GTK_DATA_PREFIX = "/run/current-system/sw";
    LD_LIBRARY_PATH = [ config.system.nssModules.path ];
  };

  nix = {
    trustedUsers = ["joerg"];
    gc.automatic = true;
    gc.dates = "03:15";
    #binaryCaches = [ https://cache.nixos.community https://cache.nixos.org/ ];
    distributedBuilds = true;
    #package = pkgs.nixUnstable;
    #buildMachines = [
    #  { hostName = "inspector.r"; sshUser = "nix"; sshKey = "/etc/nixos/secrets/id_buildfarm"; system = "x86_64-linux"; maxJobs = 8; }
    #];
    nixPath = [
      "nixpkgs=/home/joerg/git/nixpkgs"
      "nixpkgs-stable=/home/joerg/git/nixpkgs/.stable"
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
    #buildMachines = [{
    #  hostName = "aarch64.nixos.community";
    #  maxJobs = 96;
    #  sshKey = "/home/joerg/.ssh/id_ecdsa";
    #  sshUser = "mic92";
    #  system = "aarch64-linux";
    #  supportedFeatures = [ "big-parallel" ];
    #}];
  };

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_DK.UTF-8";
  };
  time.timeZone = "Europe/London";
  #time.timeZone = "Europe/Berlin";

  services = {
    #teamviewer.enable = true;
    gpm.enable = true;
    physlock.enable = true;
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
    openssh = {
      enable = true;
      forwardX11 = true;
    };

    xserver = {
      desktopManager.xterm.enable = false;
      displayManager.lightdm = {
        enable = true;
        autoLogin.user = "joerg";
        autoLogin.enable = true;
      };
      enable = true;
      layout = "us";
      xkbVariant = "altgr-intl";
      xkbOptions = "caps:escape,compose:menu";
      libinput.enable = true;

      windowManager = {
        awesome = {
          enable = true;
          luaModules = with pkgs.lua52Packages; [ luasocket cjson ];
        };
        i3 = {
          enable = false;
          extraPackages = with pkgs; [ dmenu i3pystatus i3lock i3status ];
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
    systemd-udev-settle.serviceConfig.ExecStart = ["" "${pkgs.coreutils}/bin/true"];
  };

  virtualisation = {
    lxc.enable = true;
    lxd.enable = true;
    rkt.enable = true;
    virtualbox.host.enable = false;
    docker = {
      enable = true;
      enableOnBoot = true;
      storageDriver = "zfs";
      #extraOptions = "--iptables=false --storage-opt=zfs.fsname=zroot/docker --userns-remap=docker";
      extraOptions = "--iptables=false --storage-opt=zfs.fsname=zroot/docker";
    };
  };

  fonts = {
    enableFontDir = true;
    #enableGhostscriptFonts = true;
  };

  programs = {
    ssh.startAgent = true;
    light.enable = true;
    adb.enable = true;
    #wireshark = {
    #  enable = true;
    #  package = pkgs.wireshark-gtk;
    #};
    bash.enableCompletion = true;
    zsh = {
      enable = true;
      promptInit = "";
    };
  };

  hardware.pulseaudio.enable = true;

  users.users = {
    joerg = {
      isNormalUser = true;
      extraGroups = ["wheel" "docker" "plugdev" "vboxusers" "adbusers" "input" "sway" "wireshark"];
      shell = "/run/current-system/sw/bin/zsh";
      uid = 1000;
    };
    root = {
      subUidRanges = [ { startUid = 200000; count = 65536; } ];
      subGidRanges = [ { startGid = 200000; count = 65536; } ];
    };
    docker = {
      subUidRanges = [ { startUid = 100000; count = 65536; } ];
      subGidRanges = [ { startGid = 100000; count = 65536; } ];
    };
  };
  users.groups.adbusers = {};
  users.users.unbound = {};

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
