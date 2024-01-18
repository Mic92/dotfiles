{ config, ... }: {
  services.owncast = {
    enable = true;
    port = 3012;
    openFirewall = true;
  };

  services.nginx.virtualHosts."owncast.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://localhost:${toString config.services.owncast.port}";
      proxyWebsockets = true;
    };
  };
}
