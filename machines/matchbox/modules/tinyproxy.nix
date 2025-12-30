{
  services.tinyproxy = {
    enable = true;
    settings = {
      Port = 8888;
      # Listen on all interfaces
      Listen = "::";
      # Only allow blob64 via wireguard
      Allow = "fd28:387a:2:b100:79b1:b783:1bf0:b283";
      # Increase timeout for slow video downloads
      Timeout = 600;
      MaxClients = 100;
      # Log to journal
      Syslog = true;
      LogLevel = "Warning";
    };
  };

  # Only allow access from wireguard
  networking.firewall.interfaces.wireguard.allowedTCPPorts = [ 8888 ];
}
