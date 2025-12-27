{
  config,
  pkgs,
  lib,
  self,
  ...
}:
# Other useful settings come from srvos's zfs module
{
  imports = [ self.inputs.srvos.nixosModules.mixins-latest-zfs-kernel ];
  config = lib.mkIf config.boot.zfs.enabled {
    environment.systemPackages = [
      pkgs.zfs-prune-snapshots
    ];
    boot.zfs.package = pkgs.zfs_unstable;
    services.zfs.autoSnapshot.enable = true;
  };
}
