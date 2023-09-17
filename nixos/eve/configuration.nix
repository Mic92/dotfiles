# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  #deployment.targetHost = "eve.thalheim.io";
  networking.hostName = "eve";
  networking.hostId = "8425e349";

  time.timeZone = "UTC";

  programs.vim.defaultEditor = true;

  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./modules/atuin.nix
    ./modules/adminer.nix
    ./modules/bitwarden.nix
    ./modules/buildbot.nix
    #./modules/buildbot/master.nix
    #./modules/buildbot/worker.nix
    ./modules/dendrite.nix
    ./modules/dovecot.nix
    ./modules/docker.nix
    ./modules/ergo.nix
    ./modules/et.nix
    ./modules/ejabberd.nix
    ./modules/gitea
    ./modules/grafana.nix
    ./modules/goatcounter.nix
    ./modules/home-assistant
    ./modules/influxdb.nix
    ./modules/harmonia.nix
    ./modules/knot
    ./modules/navidrome.nix
    ./modules/unbound.nix
    ./modules/mastodon-hnbot.nix
    ./modules/network.nix
    ./modules/nextcloud.nix
    ./modules/nginx/default.nix
    ./modules/openttd
    ./modules/packages.nix
    ./modules/postfix.nix
    ./modules/postgresql.nix
    ./modules/shadowsocks.nix
    ./modules/snappymail.nix
    ./modules/renovate
    ./modules/remote-builder.nix
    ./modules/redis.nix
    ./modules/rspamd/rspamd.nix
    ./modules/shiori
    ./modules/sshd.nix
    ./modules/squid.nix
    ./modules/syncthing.nix
    ./modules/teamspeak.nix
    ./modules/tt-rss.nix
    ./modules/tts.nix
    ./modules/users.nix
    ./modules/wireguard.nix
    ./modules/wiki-backup.nix
    ./modules/nixos-wiki
    ./modules/onlyoffice.nix
    #./modules/headscale.nix
    #./modules/wiregrill

    ../modules/borgbackup.nix
    ../modules/nncp.nix
    #../modules/tailscale.nix
    ../modules/builder.nix
    ../modules/iperf.nix
    ../modules/mosh.nix
    ../modules/openldap
    ../modules/rtorrent.nix
    ../modules/samba-dl.nix
    ../modules/tracing.nix
    ../modules/promtail.nix
    #../modules/wezterm.nix
    ../modules/zsh.nix
    ../modules/uptermd.nix
    ../modules/nix-ld.nix
    ../modules/zerotier.nix
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "21.11";
}
