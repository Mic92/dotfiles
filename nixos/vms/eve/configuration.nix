# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

let
  sshKeys = (import ./ssh-keys.nix);
  network = (import ./network.nix) {inherit lib;};

in {
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./network-configuration.nix
      ./lxc.nix
      ./backup.nix
      ./packages.nix
      #./telegraf.nix
      ./modules/containers.nix
      ./containers.nix
      ./resolver.nix
    ];

  boot = {
    kernel.sysctl = {
      "fs.inotify.max_queued_events"=1048576;
      "fs.inotify.max_user_instances"=1048576;
      "fs.inotify.max_user_watches"=1048576;
      "net.ipv6.conf.all.forwarding"=1;
    };
    zfs.enableUnstable = true;
    loader.grub = {
      enable = true;
      version = 2;
      device = "/dev/sda";
    };
    kernelModules = ["ip6_gre"];
    initrd.network = {
      enable = true;
      ssh = {
        enable = true;
        port = 2222;
        hostECDSAKey = /etc/nixos/secrets/initrd-ssh-key;
      };
      postCommands = ''
        echo "cryptsetup-askpass" >> /root/.profile

        ip addr add ${network.ipv6} dev eth0
        ip route add default via fe80::1 dev eth0
      '';
    };

    blacklistedKernelModules = [ "iptable_nat" "ip_tables" ];
  };

  networking = {
    hostName = "eve";
    hostId = "8425e349";
  };

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_DK.UTF-8";
  };

  time.timeZone = "UTC";

  nix = {
    gc = {
      automatic = true;
      dates = "03:15";
    };
    nixPath = [ "/etc/nixos" "nixos-config=/etc/nixos/configuration.nix"];
    binaryCaches = [ https://cache.nixos.org/ ];
    extraOptions = ''
      gc-keep-outputs = true
      gc-keep-derivations = true
      binary-caches-parallel-connections = 3
      build-max-jobs = 9
      connect-timeout = 5
    '';
  };

  nixpkgs.config.allowUnfree = true;

  environment.extraInit = ''
    export PATH="/etc/nixos/scripts:$PATH"
  '';

  programs = {
    sysdig.enable = true;
    bcc.enable = true;
    zsh = {
      enable = true;
      enableCompletion = true;
    };
  };

  virtualisation.docker = {
    enable = true;
    storageDriver = "zfs";
    extraOptions = "--iptables=false -D";
  };

  services = {
    xserver = {
      enable = true;
      desktopManager.xfce.enable = true;
    };
    xrdp = {
      enable = true;
      defaultWindowManager = "xfce4-session";
    };
    zfs.autoSnapshot.enable = true;
    openssh = {
      enable = true;
      ports = [22022];
    };
    timesyncd.enable = true;
    nscd.enable = false;
    resolved.enable = false;
    vnstat.enable = true;
  };

  users.extraUsers = {
    devkid = {
      isNormalUser = true;
      uid = 2002;
      extraGroups = ["wheel"];
      shell = "/run/current-system/sw/bin/zsh";
      openssh.authorizedKeys.keys = with sshKeys; alfred ++ [''
          command="${pkgs.borgbackup}/bin/borg serve --restrict-to-path /data/backup/devkid/pi0",no-pty,no-agent-forwarding,no-port-forwarding,no-X11-forwarding,no-user-rc ${alfredsPi}
        ''
        ''
          command="${pkgs.borgbackup}/bin/borg serve --restrict-to-path /data/backup/devkid/Dokumente --restrict-to-path /data/backup/devkid/Bilder",no-pty,no-agent-forwarding,no-port-forwarding,no-X11-forwarding,no-user-rc ${alfredsNas}
        ''
      ];
    };
    joerg = {
      isNormalUser = true;
      uid = 2003;
      extraGroups = ["wheel"];
      shell = "/run/current-system/sw/bin/zsh";
      openssh.authorizedKeys.keys = sshKeys.joerg;
    };
    root.openssh.authorizedKeys.keys = with sshKeys; alfred ++ joerg;
  };

  security = {
    sudo.wheelNeedsPassword = false;
    audit.enable = false;
    apparmor.enable = true;
  };

  systemd.timers = {
    "matemat-stats" = {
      wantedBy = ["multi-user.target"];
      timerConfig = {
        OnBootSec="20min";
        OnUnitActiveSec="60min";
      };
    };
  };

  systemd.services.matemat-stats = let
    matematEnv = pkgs.python3.buildEnv.override {
      extraLibs = with pkgs.python3Packages; [ requests influxdb ];
    };
  in {
    serviceConfig = {
      User = "nobody";
      ExecStart = "${matematEnv}/bin/python ${/etc/nixos/scripts/matemat-stats.py}";
    };
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.nixos.stateVersion = "18.03";
}
