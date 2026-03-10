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
    ./users.nix
    ../../nixosModules/users.nix
  ];

  networking.hostName = "utm-vm";
  time.timeZone = "UTC";

  nixpkgs.pkgs = self.inputs.nixpkgs.legacyPackages.aarch64-linux;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  services.getty.autologinUser = "root";

  # envfs (FUSE) causes "Freezing execution" on this VM
  services.envfs.enable = false;

  # Networking via systemd-networkd (DHCP on virtio NIC)
  systemd.network.networks.ethernet = {
    matchConfig.Type = "ether";
    networkConfig = {
      DHCP = true;
      IPv6AcceptRA = true;
    };
  };

  environment.systemPackages = with pkgs; [
    ghostty.terminfo
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
