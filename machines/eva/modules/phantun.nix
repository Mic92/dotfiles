# Phantun server for blob64 WireGuard tunnel
# Listens on TCP 4568 and forwards to local WireGuard UDP port
{
  imports = [ ../../../nixosModules/phantun ];

  services.phantun.server.blob64-wg = {
    enable = true;
    listenPort = 4568;
    remoteUdp = "127.0.0.1:51820"; # Forward to WireGuard port
    interface = "eth0";
    tun = "phantun1";
    tunLocalAddress = "192.168.203.1";
    tunPeerAddress = "192.168.203.2";
  };
}
