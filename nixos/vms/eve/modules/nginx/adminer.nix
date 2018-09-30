{ pkgs, ... }: {
  services.nginx = {
    virtualHosts."adminer.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      extraConfig = ''
        index index.php;
      '';
      locations."/".extraConfig = ''
        try_files $uri $uri/ /index.php?$args;
      '';
      locations."~* ^.+\.php$".extraConfig = ''
        include ${pkgs.nginx}/conf/fastcgi_params;
        fastcgi_pass 172.23.75.14:9000;
        fastcgi_index index.php;
        fastcgi_param SCRIPT_FILENAME $document_root/$fastcgi_script_name;
      '';
      root = "/usr/share/webapps/adminer";
    };
    virtualHosts."adminer.higgsboson.tk" = {
      useACMEHost = "higgsboson.tk";
      forceSSL = true;
      globalRedirect = "adminer.thalheim.io";
    };
  };
}
