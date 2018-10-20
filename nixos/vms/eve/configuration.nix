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
    ./modules/docker.nix
    ./modules/rsyncd.nix
    ./modules/icinga-sync.nix
    ./modules/users.nix
    ./modules/network.nix
    ./modules/zsh.nix
    ./modules/openssh.nix

    ../modules/tracing.nix
    ../modules/nix-daemon.nix
    ../modules/zfs.nix
    ../modules/nur.nix
  ];

  i18n.defaultLocale = "en_DK.UTF-8";

  time.timeZone = "UTC";

  security.audit.enable = false;

  networking.hostName = "eve";
  networking.hostId = "8425e349";

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "18.03";
}
