{ config, ... }: {
  services.owncast = {
    enable = true;
    port = "3012";
    openFirewall = true;
  };

  services.nginx.virtualHosts."owncast.thalheim.io" = {
    enableACME = true;
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://localhost:${config.services.owncast.port}";
      proxyWebsockets = true;
    };
  };
}
