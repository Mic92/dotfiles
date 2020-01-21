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
    ./modules/packages.nix
    ./modules/networkmanager.nix

    #../modules/libvirt.nix
    ((toString <nixos-hardware>) + "/lenovo/thinkpad/x250")
    ./modules/dice.nix
    ./modules/backup.nix
    ./modules/nfs.nix
    ./modules/retiolum.nix
    ../modules/zfs.nix
    #../modules/sway.nix
    ../modules/mosh.nix
    ../modules/tracing.nix
    ../modules/tor-ssh.nix
    ../modules/nix-daemon.nix
    ../modules/networkd.nix
    ../modules/dnscrypt.nix
    #./vm/modules/dnsmasq.nix
    ../modules/wireguard.nix
    ../modules/secrets.nix
    #./kde.nix
    ../modules/i3.nix
    #../modules/awesome.nix
    ../modules/pki
    ../modules/yubikey.nix
  ];

  boot = {
    loader = {
      systemd-boot.enable = true;
      # when installing toggle this
      efi.canTouchEfiVariables = false;
    };

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

  # when I need dhcp
  systemd.network.networks."eth0".extraConfig = ''
    [Match]
    Name = eth0

    [Network]
    Address = 192.168.43.1/24
    DHCPServer = yes
  '';

  environment.sessionVariables = {
    LD_LIBRARY_PATH = [ config.system.nssModules.path ];
  };

 nix = {
   binaryCaches = [
     https://r-ryantm.cachix.org
   ];
   binaryCachePublicKeys = [
     "r-ryantm.cachix.org-1:gkUbLkouDAyvBdpBX0JOdIiD2/DP1ldF3Z3Y6Gqcc4c="
   ];
   distributedBuilds = true;
   buildMachines = [
     {
       hostName = "martha.r";
       sshUser = "nix";
       sshKey = "/var/src/secrets/id_buildfarm";
       system = "x86_64-linux";
       maxJobs = 8;
     }
     {
       hostName = "donna.r";
       sshUser = "nix";
       sshKey = "/var/src/secrets/id_buildfarm";
       system = "x86_64-linux";
       maxJobs = 8;
     }
     {
       hostName = "amy.r";
       sshUser = "nix";
       sshKey = "/var/src/secrets/id_buildfarm";
       system = "x86_64-linux";
       maxJobs = 8;
     }
     {
       hostName = "clara.r";
       sshUser = "nix";
       sshKey = "/var/src/secrets/id_buildfarm";
       system = "x86_64-linux";
       maxJobs = 8;
     }
     {
       hostName = "rose.r";
       sshUser = "nix";
       sshKey = "/var/src/secrets/id_buildfarm";
       system = "x86_64-linux";
       maxJobs = 8;
     }
     {
       hostName = "prism.r";
       sshUser = "Mic92";
       sshKey = "/root/.ssh/id_ed25519";
       system = "x86_64-linux";
       maxJobs = 4;
     }
     {
       hostName = "eve.thalheim.io";
       sshUser = "nix";
       sshKey = "/var/src/secrets/id_buildfarm";
       system = "x86_64-linux";
       maxJobs = 4;
     }
     {
       hostName = "inspector.r";
       sshUser = "nix";
       sshKey = "/var/src/secrets/id_buildfarm";
       system = "x86_64-linux";
       maxJobs = 4;
     }
     {
       hostName = "dpdkm.r";
       sshKey = "/var/src/secrets/id_buildfarm";
       sshUser = "nix";
       system = "x86_64-linux";
       maxJobs = 4;
     }
     #{
     #  hostName = "eddie.r";
     #  sshKey = "/var/src/secrets/id_buildfarm";
     #  sshUser = "nix";
     #  system = "x86_64-linux";
     #  maxJobs = 2;
     #}
     # rpi3
     #{
     #  hostName = "172.23.75.254";
     #  maxJobs = 4;
     #  sshKey = "/var/src/secrets/id_buildfarm";
     #  sshUser = "nix";
     #  system = "aarch64-linux";
     #}
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

  console.keyMap = "us";
  i18n.defaultLocale = "en_DK.UTF-8";

  # Manual
  time.timeZone = null;

  services = {
    fwupd.enable = true;
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
        ExecStart = ''${pkgs.caddy}/bin/caddy -conf=${cfg} -agree'';
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

  users.users = {
    joerg = {
      isNormalUser = true;
      extraGroups = [
        "wheel" "docker" "plugdev" "vboxusers" "adbusers" "input" "sway" "wireshark" "networkmanager"
      ];
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
  #services.teamviewer.enable = true;

  nixpkgs.config = {
    allowUnfree = true;
    android_sdk.accept_license = true;
  };

  services.ssmtp = {
    enable = true;
    authPassFile = "/var/src/secrets/smtp-authpass";
    authUser = "joerg@higgsboson.tk";
    hostName = "mail.thalheim.io:587";
    domain = "thalheim.io";
    root = "joerg@thalheim.io";
    useSTARTTLS = true;
  };


  networking = {
    nameservers = [ "1.1.1.1" ];

    firewall.enable = true;
    firewall.allowedTCPPorts = [ 3030 ];
    hostName = "turingmachine";
  };

  services.tor.client.enable = true;

  system.stateVersion = "18.03";
  services.resolved.enable = false;

  networking.networkmanager.wifi.backend = "iwd";
}
