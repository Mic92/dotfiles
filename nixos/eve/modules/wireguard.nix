{ config, pkgs, ... }:
let
  listenPort = 51820;
in
{
  environment.systemPackages = with pkgs; [ wireguard-tools ];
  networking.firewall.allowedUDPPorts = [ listenPort ];

  networking.wireguard.interfaces.wg-sicily = {
    inherit listenPort;
    ips = [ "fd98:c3d0:bec8:0::1/64" ];

    privateKeyFile = config.sops.secrets.eve-wireguard.path;
    generatePrivateKeyFile = true;
  };
}
