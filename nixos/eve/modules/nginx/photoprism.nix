{
  services.nginx = {
    enable = true;
    virtualHosts."photoprism.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://blob64.r:2342";
        proxyWebsockets = true;
      };
    };
  };
}

