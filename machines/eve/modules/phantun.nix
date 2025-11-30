# Phantun server for blob64 WireGuard tunnel
# Listens on TCP 4567 and forwards to local WireGuard UDP port
{
  imports = [ ../../../nixosModules/phantun ];

  services.phantun.server.blob64-wg = {
    enable = true;
    listenPort = 4567;
    remoteUdp = "127.0.0.1:51821"; # Forward to existing WireGuard port
    interface = "enp35s0";
    tun = "phantun0";
    tunLocalAddress = "192.168.201.1";
    tunPeerAddress = "192.168.201.2";
  };
}
