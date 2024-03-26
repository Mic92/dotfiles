{ pkgs, ... }:
{
  environment.systemPackages = [
    pkgs.bcachefs-tools
  ];
  clan.localbackup.targets.hdd = {
    directory = "/backup-2tb/turingmachine";
    mountpoint = "/backup-2tb/turingmachine";
    mountHook = ''
      set -x
      bcache unlock /dev/disk/by-partuuid/b315b307-bc54-4918-8ac4-5dd99c68fa70
    '';
  };
  fileSystems."/backup-2tb/turingmachine" = {
    device = "/dev/disk/by-partuuid/b315b307-bc54-4918-8ac4-5dd99c68fa70";
    fsType = "bcachefs";
    options = [ "defaults" "noauto" ];
  };
}
