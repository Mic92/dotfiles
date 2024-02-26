# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ lib, ... }: {
  networking.hostName = "eve";
  clan.networking.targetHost = "root@eve.i";
  networking.hostId = "8425e349";

  time.timeZone = "UTC";

  programs.vim.defaultEditor = true;

  srvos.boot.consoles = lib.mkDefault [ ];

  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./modules/adminer.nix
    ./modules/atuin.nix
    ./modules/bitwarden.nix
    ./modules/buildbot.nix
    ./modules/dendrite.nix
    ./modules/disko.nix
    ./modules/docker.nix
    ./modules/dovecot.nix
    ./modules/ejabberd.nix
    ./modules/ergo.nix
    ./modules/et.nix
    ./modules/gitea
    ./modules/goatcounter.nix
    ./modules/grafana.nix
    ./modules/harmonia.nix
    ./modules/home-assistant
    #./modules/kanidm.nix
    ./modules/knot
    ./modules/mastodon-hnbot.nix
    ./modules/navidrome.nix
    ./modules/network.nix
    ./modules/nextcloud.nix
    ./modules/nginx/default.nix
    #./modules/nixos-wiki
    ./modules/openttd
    ./modules/owncast.nix
    ./modules/packages.nix
    ./modules/postfix.nix
    ./modules/postgresql.nix
    ./modules/redis.nix
    ./modules/remote-builder.nix
    ./modules/renovate
    ./modules/rspamd/rspamd.nix
    ./modules/shadowsocks.nix
    ./modules/shiori
    ./modules/snappymail.nix
    ./modules/sshd.nix
    ./modules/syncthing.nix
    ./modules/teamspeak.nix
    ./modules/tt-rss.nix
    ./modules/unbound.nix
    ./modules/users.nix
    ./modules/wireguard.nix
    ./modules/zerotier.nix

    ../modules/borgbackup.nix
    ../modules/builder.nix
    ../modules/iperf.nix
    ../modules/mosh.nix
    ../modules/nix-ld.nix
    ../modules/nncp.nix
    ../modules/openldap
    ../modules/promtail.nix
    ../modules/rtorrent.nix
    ../modules/samba-dl.nix
    ../modules/tracing.nix
    ../modules/uptermd.nix
    ../modules/zsh.nix
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "23.11";
}
