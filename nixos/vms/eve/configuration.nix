# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  deployment.targetHost = "eve.higgsboson.tk";

  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./modules/adminer.nix
    ./modules/borgbackup.nix
    ./modules/packages.nix
    ./modules/nft.nix
    ./modules/gogs.nix
    ./modules/openldap.nix
    ./modules/grafana.nix
    ./modules/influxdb.nix
    ./modules/prosody.nix
    ./modules/postfix.nix
    ./modules/postgresql.nix
    ./modules/rspamd.nix
    ./modules/dovecot.nix
    ./modules/nginx/default.nix
    ./modules/tor.nix
    ./modules/tt-rss.nix
    ./modules/nextcloud.nix
    ./modules/rainloop.nix
    ./modules/mastodon-hnbot.nix
    ./modules/phpldapadmin.nix
    ./modules/named.nix
    ./modules/syncthing.nix
    ./modules/squid.nix
    ./modules/mediawiki.nix
    ./modules/teamspeak.nix
    ./modules/netdata.nix
    ./modules/retiolum.nix
    ./modules/wireguard.nix
    ./modules/phpfpm.nix

    ../modules/tracing.nix
    ../modules/nix-daemon.nix
  ];


  nixpkgs.config.packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      inherit pkgs;
    };
  };

  boot = {
    zfs.enableUnstable = true;

    loader.grub = {
      enable = true;
      version = 2;
      device = "/dev/sda";
    };

    blacklistedKernelModules = [ "iptable_nat" "ip_tables" ];
  };


  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_DK.UTF-8";
  };

  time.timeZone = "UTC";

  programs = {
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
      ports = [
        22022 # legacy
        22
      ];
    };
    resolved.enable = false;
    vnstat.enable = true;
  };

  users.extraUsers = let
    sshKeys = (import ./ssh-keys.nix);
  in {
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

  networking = {
    hostName = "eve";
    hostId = "8425e349";
    dhcpcd.enable = false;
    # use nftables instead
    firewall.enable = false;
    nameservers = [ "127.0.0.1" ];
  };

  systemd.network = {
    enable = true;
    networks."eth0".extraConfig = ''
      [Match]
      Name = eth0

      [Network]
      DHCP = ipv4
      Address = 2a03:4000:13:31e::1/128
      Address = 2a03:4000:13:31e:1::10/128
      Address = 2a03:4000:13:31e:1::5/128
      Address = 2a03:4000:13:31e:1::6/128
      Gateway = fe80::1
      IPv6AcceptRA = no
      IPForward = yes

      [DHCP]
      UseDNS = no
    '';
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "18.03";
}
