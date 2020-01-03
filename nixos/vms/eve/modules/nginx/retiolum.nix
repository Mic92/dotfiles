{
  services.nginx.virtualHosts."retiolum.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    root = "/var/lib/gogs/builds/retiolum.thalheim.io";
  };
}
