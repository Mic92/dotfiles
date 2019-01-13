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
      rev = "1e2c130d38d72860660474c36207b099c519cb6a";
    } + "/lenovo/thinkpad/x250")
    ./dice.nix
    ./backup.nix
    ./nfs.nix
    ./vms/modules/zfs.nix
    ./vms/modules/mosh.nix
    ./vms/modules/tracing.nix
    ./vms/modules/tor-ssh.nix
    ./vms/modules/nix-daemon.nix
    ./vms/modules/retiolum.nix
    ./vms/modules/networkd.nix
    ./vms/modules/dnscrypt.nix
    ./vms/modules/wireguard.nix
    #./vms/modules/secrets.nix
    #./kde.nix
    ./vms/modules/awesome.nix
  ];

  boot = {
    loader = {
      systemd-boot.enable = true;
      # when installing toggle this
      # efi.canTouchEfiVariables = true;
    };
    blacklistedKernelModules = [ "iptable_nat" "ip_tables" ];

  };

  # when I need dhcp
  #systemd.network.networks."eth0".extraConfig = ''
  #  [Match]
  #  Name = eth0

  #  [Network]
  #  Address = 192.168.43.1/24
  #  DHCPServer = yes
  #'';

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
        hostName = "prism.r";
        sshKey = "/root/.ssh/id_ed25519";
        sshUser = "Mic92";
        system = "x86_64-linux";
        maxJobs = 4;
      }
      {
        hostName = "inspector.r";
        sshUser = "nix";
        sshKey = "/etc/nixos/secrets/id_buildfarm";
        system = "x86_64-linux";
        maxJobs = 8;
        supportedFeatures = [ "big-parallel" ];
      }
      {
        hostName = "eddie.r";
        sshKey = "/etc/nixos/secrets/id_buildfarm";
        sshUser = "nix";
        system = "x86_64-linux";
        maxJobs = 2;
      }
      {
        hostName = "dpdkm.r";
        sshKey = "/etc/nixos/secrets/id_buildfarm";
        sshUser = "nix";
        system = "x86_64-linux";
        maxJobs = 4;
      }
      {
        hostName = "172.23.75.254";
        maxJobs = 4;
        sshKey = "/etc/nixos/secrets/id_buildfarm";
        sshUser = "nix";
        system = "aarch64-linux";
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

  nixpkgs.config = {
    allowUnfree = true;
    android_sdk.accept_license = true;
  };

  networking = {
    networkmanager.enable = true;

    retiolum = {
      ipv4 = "10.243.29.168";
      ipv6 = "42:0:3c46:47e8:f610:15d1:27a3:674b";
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
}
