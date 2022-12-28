{ pkgs, ... }: {
  boot.zfs.enableUnstable = true;
  boot.zfs.requestEncryptionCredentials = true;
  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "ac174b52";
  environment.systemPackages = [
    pkgs.mbuffer # for sending
  ];
}
