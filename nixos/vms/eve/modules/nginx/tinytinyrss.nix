{ pkgs, ... }:

let
  page = acmeHost: {
    useACMEHost = acmeHost;
    forceSSL = true;
    root = "/usr/share/webapps/tt-rss";
    locations."~ \.php$".extraConfig = ''
      include ${pkgs.nginx}/conf/fastcgi_params;
      try_files $uri = 404;
      fastcgi_pass 172.23.75.20:9000;
      fastcgi_index index.php;
      fastcgi_param SCRIPT_FILENAME $document_root/$fastcgi_script_name;
    '';
  };
in {
  services.nginx = {
    virtualHosts."rss.higgsboson.tk" = {
      useACMEHost = "higgsboson.tk";
      forceSSL = true;
      globalRedirect = "rss.thalheim.io";
    };

    virtualHosts."rss.thalheim.io" = page "thalheim.io";
    virtualHosts."rss.devkid.net" = page "devkid.net";
  };
}
