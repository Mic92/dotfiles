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
  config = {
    environment.systemPackages = lib.mkIf (config.boot.zfs.enabled) [
      pkgs.zfs-prune-snapshots
    ];
    boot.zfs.package = pkgs.zfsUnstable;
  };
}