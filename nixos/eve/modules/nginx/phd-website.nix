{
  services.nginx = {
    virtualHosts."phd.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      root = "/var/www/phd-website";
      extraConfig = ''
        charset utf-8;
        source_charset utf-8;
      '';
    };

    virtualHosts."www.phd.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      globalRedirect = "phd.thalheim.io";
    };
  };

  systemd.tmpfiles.rules = [
    "d /var/www/phd-website 0755 syncthing users - -"
  ];
}
