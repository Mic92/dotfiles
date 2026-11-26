{
  pkgs,
  self,
  ...
}:
{
  imports = [
    self.nixosModules.default
    self.inputs.srvos.nixosModules.server
    self.inputs.srvos.nixosModules.mixins-systemd-boot
    ../../nixosModules/users.nix
    ../../nixosModules/minimal-docs.nix
    ../../nixosModules/networkd.nix
    ../../nixosModules/ip-update.nix
    ../../nixosModules/packages.nix
    ../../nixosModules/radicle-mic92.nix
    ./modules/photoprism.nix
    ./modules/phantun.nix
  ];

  nixpkgs.pkgs = self.inputs.nixpkgs.legacyPackages.x86_64-linux;

  networking.hostName = "blob64";

  clan.core.networking.buildHost = "root@eve.i";

  system.stateVersion = "23.11";

  services.openssh.enable = true;

  environment.systemPackages = [ pkgs.smartmontools ];

  systemd.network.networks."10-uplink" = {
    matchConfig.Type = "ether";
    networkConfig.DHCP = "yes";
  };

  boot.loader.efi.canTouchEfiVariables = true;

  # Same id as the previous blob64 hardware, so zdata imports without force.
  networking.hostId = "ac174b52";

  # The HDD pool is not managed by disko: it predates this machine.
  # Mounted by zfs-mount.service; an additional fstab entry races with it.
  boot.zfs.extraPools = [ "zdata" ];

}
