{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
# Other useful settings come from srvos's zfs module
{
  imports = [ inputs.srvos.nixosModules.mixins-latest-zfs-kernel ];
  config = lib.mkIf config.boot.zfs.enabled {
    environment.systemPackages = [ pkgs.zfs-prune-snapshots ];
    boot.zfs.package = pkgs.zfsUnstable;
  };
}
