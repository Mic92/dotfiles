{ config, pkgs, lib, ... }:
# Other useful settings come from srvos's zfs module
{
  config = lib.mkIf config.boot.zfs.enabled {
    environment.systemPackages = [
      pkgs.zfs-prune-snapshots
    ];

    boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
    boot.zfs.removeLinuxDRM = pkgs.hostPlatform.isAarch64;
  };
}
