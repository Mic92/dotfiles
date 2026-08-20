{
  services.nginx.virtualHosts."retiolum.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    root = "/var/www/retiolum.thalheim.io";
  };
}
