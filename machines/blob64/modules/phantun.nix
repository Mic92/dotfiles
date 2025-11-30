# Phantun client for eve WireGuard tunnel
# Connects to eve's phantun server and exposes local UDP endpoint
{ lib, ... }:
{
  imports = [ ../../../nixosModules/phantun ];

  services.phantun.client.eve-wg = {
    enable = true;
    localUdp = "127.0.0.1:51821"; # Local UDP endpoint matching WireGuard port
    remoteAddress = "95.217.199.121:4567"; # Eve's public IP (phantun uses TCP)
    interface = "end0"; # blob64's network interface
    tun = "phantun0";
    tunLocalAddress = "192.168.200.1";
    tunPeerAddress = "192.168.200.2";
  };

  # Override wireguard endpoint for eve to use phantun tunnel
  networking.wireguard.interfaces.wireguard.peers = lib.mkForce [
    {
      # eva - unchanged
      publicKey = "n3jAKetBA7w0usNDIuN2+KvldFRZwrz7rJCS/AcKXFQ=";
      allowedIPs = [ "fd28:387a:2:b100::/56" ];
      endpoint = "eva.i:51820";
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
