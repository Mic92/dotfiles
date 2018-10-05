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
    ./packages.nix
    #./libvirt.nix
    (builtins.fetchGit {
      url = "https://github.com/NixOS/nixos-hardware";
      rev = "cfc72f6c9b4348aa9952730c52c44cf8460fbc40";
    } + "/lenovo/thinkpad/x250") 
    ./dice.nix
    ./backup.nix
    ./nfs.nix
    ./vms/modules/zfs.nix
    ./vms/modules/mosh.nix
    ./vms/modules/overlay.nix
    ./vms/modules/tracing.nix
    ./vms/modules/tor-ssh.nix
    ./vms/modules/nix-daemon.nix
    ./vms/modules/retiolum.nix
    ./vms/modules/networkd.nix
    ./kde.nix
  ];

  boot = {
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
    binaryCaches = [
      https://cache.nixos.org/
      https://r-ryantm.cachix.org
    ];
    binaryCachePublicKeys = [
      "r-ryantm.cachix.org-1:gkUbLkouDAyvBdpBX0JOdIiD2/DP1ldF3Z3Y6Gqcc4c="
    ];
    distributedBuilds = true;
    buildMachines = [
      { 
        hostName = "inspector.r";
        sshUser = "nix"; 
        sshKey = "/etc/nixos/secrets/id_buildfarm"; 
        system = "x86_64-linux"; 
        maxJobs = 16;
        supportedFeatures = [ "big-parallel" ];
      }
      {
        hostName = "dpdkm.r";
        sshKey = "/etc/nixos/secrets/id_buildfarm"; 
        sshUser = "nix";
        system = "x86_64-linux"; 
        maxJobs = 8;
      }
      {
        hostName = "eddie.r";
        sshKey = "/etc/nixos/secrets/id_buildfarm"; 
        sshUser = "nix";
        system = "x86_64-linux"; 
        maxJobs = 8;
      }
      {
        hostName = "aarch64.nixos.community";
        maxJobs = 96;
        sshKey = "/root/.ssh/id_ed25519";
        sshUser = "mic92";
        system = "aarch64-linux";
        supportedFeatures = [ "big-parallel" ];
      }
    ];
    nixPath = [
      "nixpkgs=/home/joerg/git/nixpkgs"
      "nixos-config=/home/joerg/git/nixos-configuration/configuration.nix"
      "nixpkgs-overlays=/home/joerg/git/nixos-configuration/overlays"
      "/nix/var/nix/profiles/per-user/root/channels"
    ];
    extraOptions = ''
      builders-use-substitutes = true
    '';
  };

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_DK.UTF-8";
  };

  time.timeZone = "Europe/London";

  services = {
    gpm.enable = true;
    autorandr.enable = true;
    resilio = {
      enable = true;
      enableWebUI = true;
    };
    upower.enable = true;
    locate.enable = true;

    openssh = {
      enable = true;
      forwardX11 = true;
    };

    xserver = {
      enable = true;
      layout = "us";
      xkbVariant = "altgr-intl";
      xkbOptions = "caps:escape,compose:menu";
      libinput.enable = true;
    };

    avahi.enable = true;

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

  #systemd.package = lib.mkForce (pkgs.systemd.overrideAttrs (old: {
  #  name = "systemd-239";
  #  src = pkgs.fetchFromGitHub { 
  #    owner = "Mic92";
  #    repo = "systemd";
  #    rev = "nixos-v239";
  #    sha256 = "114vq71gcddi4qm2hyrj5jsas9599s0h5mg65jfpvxhfyaw54cpv";
  #  };
  #  patches = [];
  #}));

  powerManagement.powertop.enable = true;

  systemd.services = {
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

    btsync = with pkgs; {
      wantedBy = [ "multi-user.target" ];
      description = "Bittorrent Sync user service";
      after = [ "network.target" "local-fs.target" ];
      enable = false;
      serviceConfig = {
        Restart = "on-abort";
        User = "joerg";
        ExecStart = "${pkgs.bittorrentSync20}/bin/btsync --nodaemon --config /home/joerg/.config/btsync/btsync.conf";
      };
    };

    wpa_supplicant = {
      serviceConfig.ExecStart = ["" "${pkgs.wpa_supplicant}/bin/wpa_supplicant -iwlan0 -u -c /etc/wpa_supplicant.conf"];
    };
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
      extraOptions = "--userland-proxy=false --ip-masq=true --storage-opt=zfs.fsname=zroot/docker";
    };
  };

  fonts.enableFontDir = true;

  programs = {
    ssh.extraConfig = ''
      SendEnv LANG LC_*
    '';
    ssh.startAgent = true;
    adb.enable = true;
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

  nixpkgs.config.allowUnfree = true;

  networking = {
    networkmanager.enable = true;

    retiolum = {
      ipv4 = "10.243.29.168";
      ipv6 = "42:4992:6a6d:600::1";
    };

    defaultMailServer = {
      directDelivery = true;
      hostName = "mail.thalheim.io:587";
      root = "joerg@thalheim.io";
      authUser = "joerg@higgsboson.tk";
      authPassFile = "/etc/nixos/secrets/smtp-authpass";
      domain = "thalheim.io";
      useSTARTTLS = true;
    };
    nameservers = ["1.1.1.1"];

    firewall.enable = true;
    firewall.allowedTCPPorts = [ 3030 ];
    hostName = "turingmachine";
  };

  system.stateVersion = "18.03";

  services.resolved.enable = false;

  #containers.database = {
  #  privateNetwork = true;
  #  hostAddress = "192.168.100.10";
  #  localAddress = "192.168.100.11";
  #  config = { config, pkgs, ... }: {
  #    system.stateVersion = "18.03";
  #    services.postgresql.enable = true;
  #    environment.systemPackages = [ pkgs.htop ];
  #  };
  #};
}
