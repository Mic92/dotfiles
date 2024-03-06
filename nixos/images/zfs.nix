{ pkgs }:
let
  zfsPackage = pkgs.zfsUnstable.override {
    removeLinuxDRM = pkgs.hostPlatform.isAarch64;
  };
in
{
  boot.zfs.package = zfsPackage;
  boot.zfs.requestEncryptionCredentials = true;
  boot.kernelPackages = zfsPackage.latestCompatibleLinuxPackages;
  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "ac174b52";
  environment.systemPackages = [
    pkgs.mbuffer # for sending
  ];
}
