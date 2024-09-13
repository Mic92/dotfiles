{
  services.nginx = {
    virtualHosts."devkid.net" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      root = "/var/www/devkid.net";
    };

    virtualHosts."blog.devkid.net" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      root = "/var/www/devkid.net";
    };

    virtualHosts."www.devkid.net" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      globalRedirect = "devkid.net";
    };
  };
}
