{ lib, pkgs, config, ... }:
{
  clan.localbackup.targets.hdd = {
    directory = "/backup-2tb/turingmachine";
    # mountpoint = "/backup-2tb/turingmachine";
    mountHook = ''
      set -x
      if [[ ! -b /dev/mapper/backup-2tb ]]; then
        ${lib.getExe pkgs.cryptsetup} open /dev/disk/by-uuid/5bddd25a-23f6-4a49-b1fe-a3f6f2977436 backup-2tb \
          --key-file /home/joerg/.secret/backup-2tb.key
      fi
      if ! ${config.boot.zfs.package}/bin/zpool list backup-2tb > /dev/null 2>&1; then
        ${config.boot.zfs.package}/bin/zpool import backup-2tb
      fi
      ${config.boot.zfs.package}/bin/zfs mount -a
    '';
    unmountHook = ''
      ${config.boot.zfs.package}/bin/zpool umount backup-2tb
      ${config.boot.zfs.package}/bin/zpool export backup-2tb
      ${lib.getExe pkgs.cryptsetup} close backup-2tb
    '';
  };
}
