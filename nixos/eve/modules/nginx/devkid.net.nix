{
  services.nginx = {
    virtualHosts."devkid.net" = {
      useACMEHost = "devkid.net";
      forceSSL = true;
      root = "/var/www/devkid.net";
    };

    virtualHosts."www.devkid.net" = {
      useACMEHost = "devkid.net";
      forceSSL = true;
      globalRedirect = "devkid.net";
    };
  };
}
