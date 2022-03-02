{pkgs, ...}: {
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

    virtualHosts."www.vmsh.org" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      extraConfig = ''
        return 302 https://github.com/Mic92/vmsh;
      '';
    };

    virtualHosts."vmsh.org" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      extraConfig = ''
        return 302 https://github.com/Mic92/vmsh;
      '';
    };
  };
}
