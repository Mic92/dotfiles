{ pkgs, config, lib, ... }:
{
  clan.localbackup.targets.hdd = {
    directory = "/backup-2tb/turingmachine";
    mountpoint = "/backup-2tb";
    preMountHook = ''
      set -x
      export PATH=${lib.makeBinPath [pkgs.bcachefs-tools pkgs.keyutils pkgs.util-linux]}
      keyctl link @u @s
      ${pkgs.bcachefs-tools}/bin/bcachefs unlock /dev/disk/by-partuuid/b315b307-bc54-4918-8ac4-5dd99c68fa70 < ${config.sops.secrets.turingmachine-bcachefs-password.path}
    '';
  };
  fileSystems."/backup-2tb" = {
    device = "/dev/disk/by-partuuid/b315b307-bc54-4918-8ac4-5dd99c68fa70";
    fsType = "bcachefs";
    options = [ "defaults" "noauto" ];
  };
}
