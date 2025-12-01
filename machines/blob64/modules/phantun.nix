# Phantun clients for WireGuard tunnels
# Connects to eve/eva's phantun servers and exposes local UDP endpoints
{ lib, ... }:
{
  imports = [ ../../../nixosModules/phantun ];

  # Phantun client for eve
  services.phantun.client.eve-wg = {
    enable = true;
    localUdp = "127.0.0.1:51821"; # Local UDP endpoint for eve
    remoteAddress = "95.217.199.121:4567"; # Eve's public IP (phantun uses TCP)
    interface = "end0"; # blob64's network interface
    tun = "phantun0";
    tunLocalAddress = "192.168.200.1";
    tunPeerAddress = "192.168.200.2";
  };

  # Phantun client for eva
  services.phantun.client.eva-wg = {
    enable = true;
    localUdp = "127.0.0.1:51822"; # Local UDP endpoint for eva
    remoteAddress = "116.203.179.132:4568"; # Eva's public IP (phantun uses TCP)
    interface = "end0"; # blob64's network interface
    tun = "phantun1";
    tunLocalAddress = "192.168.202.1";
    tunPeerAddress = "192.168.202.2";
  };

  # Override wireguard endpoints to use phantun tunnels
  networking.wireguard.interfaces.wireguard.peers = lib.mkForce [
    {
      # eva - via phantun tunnel
      publicKey = "n3jAKetBA7w0usNDIuN2+KvldFRZwrz7rJCS/AcKXFQ=";
      allowedIPs = [ "fd28:387a:2:b100::/56" ];
      endpoint = "127.0.0.1:51822"; # Via phantun
      persistentKeepalive = 25;
    }
    {
      # eve - via phantun tunnel
      publicKey = "/Deus55uMwnvhHVNYUWpkufilTfeAoj/KBeGWaha1w4=";
      allowedIPs = [ "fd28:387a:85:2600::/56" ];
      endpoint = "127.0.0.1:51821"; # Via phantun
      persistentKeepalive = 25;
    }
  ];
}
