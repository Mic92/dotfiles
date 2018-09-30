{
  services.nginx = {
    virtualHosts."halfco.de" = {
      useACMEHost = "halfco.de";
      forceSSL = true;
      root = "/var/www/halfco.de";
    };
    virtualHosts."www.halfco.de" = {
      useACMEHost = "halfco.de";
      forceSSL = true;
      globalRedirect = "halfco.de";
    };
  };
}
