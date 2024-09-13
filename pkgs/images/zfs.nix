{
  config,
  lib,
  pkgs,
  ...
}:
{
  # You may also find this setting useful to automatically set the latest compatible kernel:
  boot.kernelPackages = lib.mkDefault config.boot.zfs.package.latestCompatibleLinuxPackages;

  # Use the same default hostID as the NixOS install ISO and nixos-anywhere.
  # This allows us to import zfs pool without using a force import.
  # ZFS has this as a safety mechanism for networked block storage (ISCSI), but
  # in practice we found it causes more breakages like unbootable machines,
  # while people using ZFS on ISCSI is quite rare.
  networking.hostId = lib.mkDefault "8425e349";

  services.zfs = lib.mkIf (config.boot.zfs.enabled) {
    autoSnapshot.enable = true;
    # defaults to 12, which is a bit much given how much data is written
    autoSnapshot.monthly = lib.mkDefault 1;
    autoScrub.enable = true;
  };

  environment.systemPackages = [ pkgs.mbuffer ]; # for sending
}
