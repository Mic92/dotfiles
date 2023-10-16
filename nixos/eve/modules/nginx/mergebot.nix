{
  services.nginx.virtualHosts."mergebot.thalheim.io" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://turingmachine.r:8080";
      proxyWebsockets = true;
      recommendedProxySettings = true;
    };
  };
}
