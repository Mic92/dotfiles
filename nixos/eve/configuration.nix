# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  #deployment.targetHost = "eve.thalheim.io";
  networking.hostName = "eve";
  networking.hostId = "8425e349";

  time.timeZone = "UTC";
  i18n.defaultLocale = "en_DK.UTF-8";

  programs.vim.defaultEditor = true;

  nixpkgs.config.php.mhash = true;

  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./modules/adminer.nix
    ./modules/bitwarden.nix
    ./modules/borgbackup.nix
    ./modules/buildkite.nix
    ./modules/choose-place
    ./modules/krops-deploy
    ./modules/docker.nix
    ./modules/dovecot.nix
    ./modules/ejabberd.nix
    ./modules/gogs.nix
    ./modules/grafana.nix
    ./modules/home-assistant
    ./modules/icinga2
    ./modules/influxdb.nix
    ./modules/knot
    ./modules/kresd.nix
    ./modules/mastodon-hnbot.nix
    ./modules/mediawiki.nix
    #./modules/named.nix
    ./modules/netdata.nix
    ./modules/network.nix
    ./modules/nextcloud.nix
    ./modules/nginx/default.nix
    ./modules/openldap.nix
    ./modules/openttd
    ./modules/packages.nix
    ./modules/phpfpm.nix
    ./modules/phpldapadmin.nix
    ./modules/postfix.nix
    ./modules/postgresql.nix
    ./modules/rainloop.nix
    ./modules/retiolum.nix
    ./modules/redis.nix
    ./modules/rspamd.nix
    ./modules/rsyncd.nix
    ./modules/sshd.nix
    ./modules/sslh.nix
    ./modules/squid.nix
    ./modules/syncthing.nix
    ./modules/smartd.nix
    ./modules/teamspeak.nix
    ./modules/tor.nix
    ./modules/tt-rss.nix
    ./modules/users.nix
    #./modules/wiregrill
    ./modules/zsh.nix

    ../modules/builder.nix
    ../modules/nix-daemon.nix
    ../modules/mosh.nix
    ../modules/nur.nix
    ../modules/secrets.nix
    ../modules/tracing.nix
    ../modules/wireguard.nix
    ../modules/zfs.nix
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "18.03";
}
