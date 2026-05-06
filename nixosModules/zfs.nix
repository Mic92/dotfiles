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
    # Single-machine laptops/desktops: allow import after rescue/installer touched
    # the pool with a different hostid, avoiding an emergency shell on next boot.
    boot.zfs.forceImportRoot = true;
    services.zfs.autoSnapshot.enable = true;
  };
}
