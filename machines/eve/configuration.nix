# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  lib,
  inputs,
  self,
  ...
}:
{
  networking.hostName = "eve";
  clan.core.networking.targetHost = "root@eve.i";
  networking.hostId = "8425e349";

  time.timeZone = "UTC";

  srvos.boot.consoles = lib.mkDefault [ ];

  imports = [
    self.nixosModules.default
    inputs.srvos.nixosModules.server
    inputs.srvos.nixosModules.mixins-nginx
    inputs.srvos.nixosModules.hardware-hetzner-online-amd
    inputs.buildbot-nix.nixosModules.buildbot-worker
    inputs.buildbot-nix.nixosModules.buildbot-master
    inputs.disko.nixosModules.disko

    inputs.nix-index-database.nixosModules.nix-index
    { programs.nix-index-database.comma.enable = true; }

    ./modules/adminer.nix
    ./modules/atuin.nix
    ./modules/bitwarden.nix
    ./modules/buildbot.nix
    ./modules/dendrite.nix
    ./modules/disko.nix
    ./modules/docker.nix
    ./modules/dovecot.nix
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
    ./modules/signald.nix
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
    #./modules/zerotier.nix

    ../modules/borgbackup.nix
    ../modules/builder.nix
    ../modules/data-mesher.nix
    ../modules/iperf.nix
    ../modules/mosh.nix
    ../modules/nncp.nix
    ../modules/openldap
    ../modules/promtail.nix
    ../modules/rtorrent.nix
    ../modules/samba-dl.nix
    ../modules/tracing.nix
    ../modules/uptermd.nix
    ../modules/zsh.nix
  ];
  nixpkgs.pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "23.11";
}
