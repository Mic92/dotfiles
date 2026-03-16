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
  networking.hostId = "8425e349";

  # Disable envfs to fix systemd refusing to run with unpopulated /usr/
  services.envfs.enable = lib.mkForce false;

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
    ./modules/authelia.nix
    ./modules/vaultwarden.nix
    ./modules/buildbot.nix
    ./modules/calendar-bot/configuration.nix
    ./modules/dendrite.nix
    ./modules/disko.nix
    ./modules/dovecot.nix
    ./modules/et.nix
    ./modules/gitea
    ./modules/goatcounter.nix
    ./modules/grafana.nix
    ./modules/harmonia.nix
    ./modules/kimai
    ./modules/mtls-cache.nix
    ./modules/knot
    ./modules/mastodon-hnbot.nix
    self.inputs.mics-n8n-nodes.nixosModules.default
    ./modules/n8n
    ./modules/network.nix
    ./modules/nostr-relay.nix
    ./modules/route96.nix
    ./modules/nextcloud.nix
    ./modules/nginx/default.nix
    ./modules/opencrow
    #./modules/nixos-wiki
    ./modules/packages.nix
    ./modules/paperless.nix
    ./modules/pinchflat.nix
    #./modules/radicle.nix
    ./modules/phpldapadmin.nix
    ./modules/postfix.nix
    ./modules/postgresql.nix
    ./modules/redis.nix
    ./modules/remote-builder.nix
    ./modules/rspamd/rspamd.nix
    ./modules/rustdesk-server.nix
    ./modules/shiori
    ./modules/snappymail.nix
    ./modules/sshd.nix
    ./modules/step-ca/default.nix
    ./modules/syncthing.nix
    ./modules/teamspeak.nix
    ./modules/telegraf.nix
    ./modules/freshrss
    ./modules/unbound.nix
    ./modules/users.nix
    ./modules/wireguard.nix
    ./modules/zerotier.nix
    ./modules/phantun.nix

    ../../nixosModules/borgbackup.nix
    ../../nixosModules/builder.nix
    ../../nixosModules/docker-zfs.nix
    ../../nixosModules/iperf.nix
    ../../nixosModules/nncp.nix
    ../../nixosModules/openldap
    ../../nixosModules/promtail.nix
    ../../nixosModules/rtorrent.nix
    ../../nixosModules/flood.nix
    ../../nixosModules/samba-dl.nix
    ../../nixosModules/tracing.nix
    ../../nixosModules/uptermd.nix
    ../../nixosModules/zsh.nix
  ];
  nixpkgs.pkgs = self.inputs.nixpkgs.legacyPackages.x86_64-linux;

  # Work around broken pam_lastlog2.so missing libpam linkage in systemd 259
  # https://github.com/NixOS/nixpkgs/issues/493934
  # TODO: remove once https://github.com/NixOS/nixpkgs/pull/495347 is merged
  containers.opencrow.config.security.pam.services.login.updateWtmp = lib.mkForce false;

  # The NixOS release to be compatible with for stateful data such as databases.
}
