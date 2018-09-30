{ pkgs, ... }: {
  services.nginx = {
    virtualHosts."mail.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      root = "/srv/http/mail.higgsboson.tk";
      locations."/".extraConfig = ''
        index index.php;
        autoindex on;
        autoindex_exact_size off;
        autoindex_localtime on;
      '';
      locations."^~ /data".extraConfig = ''
        deny all;
      '';
      locations."~ \.php$".extraConfig = ''
        include ${pkgs.nginx}/conf/fastcgi_params;
        fastcgi_param   SCRIPT_FILENAME $document_root$fastcgi_script_name;
        fastcgi_pass 172.23.75.19:9000;
      '';
    };
    virtualHosts."mail.higgsboson.tk" = {
      useACMEHost = "higgsboson.tk";
      forceSSL = true;
      globalRedirect = "mail.thalheim.io";
    };
  };
}
