{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [ wireguard-tools ];
  networking.firewall.allowedUDPPorts = [ 51820 ];
}
