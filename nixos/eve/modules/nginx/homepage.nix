{
  services.nginx = {
    virtualHosts."boot.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      root = "/var/www/boot.thalheim.io";
    };

    virtualHosts."thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      root = "/var/www/higgsboson.tk";
      locations."/http-bind".extraConfig = ''
        proxy_pass  http://localhost:5280/http-bind;
        proxy_set_header Host $host;
        proxy_buffering off;
        tcp_nodelay on;
      '';
      extraConfig = ''
        charset utf-8;
        source_charset utf-8;
      '';
    };

    virtualHosts."www.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      globalRedirect = "thalheim.io";
    };

    virtualHosts."reports.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      root = "/var/www/reports.thalheim.io";
      extraConfig = ''
        index index.html;
        charset utf-8;
        source_charset utf-8;
      '';
    };
  };

  systemd.tmpfiles.rules = [
    "d /var/www/reports.thalheim.io 0750 joerg nginx - -"
  ];
}
