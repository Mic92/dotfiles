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
    peers = [{
      allowedIPs = [ "fd98:c3d0:bec8:0::2/128" ];
      publicKey = "RTOE/69lnlAFwIXiFJAn+Q90ON4TfFdu417yJcpCjCM=";
    }];
  };
}
