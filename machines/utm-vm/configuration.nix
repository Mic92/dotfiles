{
  pkgs,
  lib,
  self,
  ...
}:
{
  imports = [
    self.nixosModules.default
    self.inputs.srvos.nixosModules.server
    self.inputs.disko.nixosModules.disko
    self.inputs.nix-index-database.nixosModules.nix-index
    { programs.nix-index-database.comma.enable = true; }

    ./disko.nix
    ../../nixosModules/users.nix
  ];

  networking.hostName = "utm-vm";
  time.timeZone = "UTC";

  nixpkgs.pkgs = self.inputs.nixpkgs.legacyPackages.aarch64-linux;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # UTM/QEMU virtio drivers
  boot.initrd.availableKernelModules = [
    "virtio_pci"
    "virtio_blk"
    "virtio_scsi"
    "virtio_net"
  ];

  # Serial console for headless UTM access
  boot.kernelParams = [ "console=ttyAMA0,115200" ];
  services.getty.autologinUser = "root";

  # Networking via systemd-networkd (DHCP on virtio NIC)
  systemd.network.networks.ethernet = {
    matchConfig.Type = "ether";
    networkConfig = {
      DHCP = true;
      IPv6AcceptRA = true;
    };
  };

  environment.systemPackages = with pkgs; [
    tmux
    htop
    iotop
    git
    vim
    strace
    tcpdump
    ethtool
    python3
    jq
    ripgrep
    fd
  ];

  nix.settings.max-jobs = lib.mkDefault 4;

  system.stateVersion = "24.11";
}
