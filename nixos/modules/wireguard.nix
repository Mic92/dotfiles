{ config, pkgs, ... }: {
  boot.extraModulePackages = [ config.boot.kernelPackages.wireguard ];
  environment.systemPackages = with pkgs; [ wireguard-tools ];
  networking.firewall.allowedUDPPorts = [ 51820 ];
}
