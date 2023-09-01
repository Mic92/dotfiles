{
  services.atuin.enable = true;
  services.atuin.openRegistration = false;
  services.nginx.virtualHosts."atuin.thalheim.io" = {
    forceSSL = true;
    useACMEHost = "thalheim.io";
    locations."/".proxyPass = "http://localhost:8888";
  };
}
