{ pkgs, ... }: {
  services.nginx = {
    virtualHosts."ldap.thalheim.io" = {
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
        fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;
        fastcgi_pass 172.23.75.12:9000;
        fastcgi_index index.php;
      '';
      root = "/usr/share/webapps/phpldapadmin";
    };
    virtualHosts."ldap.higgsboson.tk" = {
      useACMEHost = "higgsboson.tk";
      forceSSL = true;
      globalRedirect = "ldap.thalheim.io";
    };
  };
}
