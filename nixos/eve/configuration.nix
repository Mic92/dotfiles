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

  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./modules/adminer.nix
    ./modules/bitwarden.nix
    ./modules/borgbackup
    ./modules/dovecot.nix
    ./modules/drone
    ./modules/docker.nix
    ./modules/ejabberd.nix
    ./modules/gitea.nix
    ./modules/gitlab-runner.nix
    ./modules/grafana.nix
    ./modules/goatcounter.nix
    ./modules/home-assistant
    ./modules/influxdb.nix
    #./modules/k8s.nix
    ./modules/knot
    ./modules/kresd.nix
    ./modules/mastodon-hnbot.nix
    ./modules/mediawiki.nix
    ./modules/network.nix
    ./modules/nextcloud.nix
    ./modules/nginx/default.nix
    ./modules/openttd
    ./modules/packages.nix
    ./modules/postfix.nix
    ./modules/postgresql.nix
    ./modules/rainloop.nix
    ./modules/retiolum.nix
    ./modules/remote-builder.nix
    ./modules/redis.nix
    ./modules/rspamd/rspamd.nix
    ./modules/sshd.nix
    ./modules/squid.nix
    ./modules/syncthing.nix
    ./modules/smartd.nix
    ./modules/sops.nix
    ./modules/teamspeak.nix
    ./modules/telegraf.nix
    ./modules/tt-rss.nix
    ./modules/users.nix
    #./modules/wiregrill

    ../modules/builder.nix
    ../modules/iperf.nix
    ../modules/mosh.nix
    ../modules/openldap
    ../modules/samba-dl.nix
    ../modules/tor-ssh.nix
    ../modules/tracing.nix
    ../modules/promtail.nix
    #../modules/wezterm.nix
    ../modules/wireguard.nix
    ../modules/zfs.nix
    ../modules/zsh.nix
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "18.03";
}
