# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  lib,
  self,
  ...
}:
{
  networking.hostName = "eve";
  clan.core.networking.targetHost = lib.mkForce "root@eve.i";
  networking.hostId = "8425e349";

  time.timeZone = "UTC";

  srvos.boot.consoles = lib.mkDefault [ ];

  imports = [
    self.nixosModules.default
    self.inputs.srvos.nixosModules.server
    self.inputs.srvos.nixosModules.mixins-nginx
    self.inputs.srvos.nixosModules.hardware-hetzner-online-amd
    self.inputs.buildbot-nix.nixosModules.buildbot-worker
    self.inputs.buildbot-nix.nixosModules.buildbot-master
    self.inputs.disko.nixosModules.disko

    self.inputs.nix-index-database.nixosModules.nix-index
    { programs.nix-index-database.comma.enable = true; }

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
    ./modules/kanidm.nix
    ./modules/knot
    ./modules/mastodon-hnbot.nix
    ./modules/network.nix
    ./modules/nextcloud.nix
    ./modules/nginx/default.nix
    #./modules/nixos-wiki
    ./modules/openttd
    ./modules/owncast.nix
    ./modules/packages.nix
    ./modules/paperless.nix
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

    ../../nixosModules/borgbackup.nix
    ../../nixosModules/builder.nix
    ../../nixosModules/iperf.nix
    ../../nixosModules/mosh.nix
    ../../nixosModules/nncp.nix
    ../../nixosModules/openldap
    ../../nixosModules/promtail.nix
    ../../nixosModules/rtorrent.nix
    ../../nixosModules/samba-dl.nix
    ../../nixosModules/tracing.nix
    ../../nixosModules/uptermd.nix
    ../../nixosModules/zsh.nix
    ../../nixosModules/hyprspace-public.nix
  ];
  nixpkgs.pkgs = self.inputs.nixpkgs.legacyPackages.x86_64-linux;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "23.11";
}
