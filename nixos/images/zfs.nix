{ pkgs, lib ... }: {
  boot.zfs.enableUnstable = true;
  boot.zfs.requestEncryptionCredentials = true;
  boot.kernelPackages = lib.mkDefault (pkgs.zfsUnstable.override {
    removeLinuxDRM = pkgs.hostPlatform.isAarch64;
  }).latestCompatibleLinuxPackages;
  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "ac174b52";
  environment.systemPackages = [
    pkgs.mbuffer # for sending
  ];
}
