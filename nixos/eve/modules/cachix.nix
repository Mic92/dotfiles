{ config, pkgs, lib, ... }:
{
  systemd.services.cachix-watch-store = {
    description = "Cachix store watcher service";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    path = [ config.nix.package ];
    # either cachix or nix want that
    environment.XDG_CACHE_HOME = "/var/cache/cachix-watch-store";
    serviceConfig = {
      Restart = "always";
      CacheDirectory = "cachix-watch-store";
      ExecStart = "${pkgs.cachix}/bin/cachix -c ${config.sops.secrets.cachix.path} watch-store mic92";
      KillSignal = "SIGINT";
    };
  };

  systemd.services.nix-gc.serviceConfig = {
    # cachix-watch-store can crash during nix-gc
    ExecStartPre = [
      "${pkgs.systemd}/bin/systemctl stop cachix-watch-store.service"
    ];
    ExecStopPost = "${pkgs.systemd}/bin/systemctl start cachix-watch-store.service";
  };

  sops.secrets.cachix = { };
}
