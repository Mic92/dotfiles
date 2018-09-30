{ pkgs, ... }:
{
  services.nginx = {
    virtualHosts."piwik.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      root = "/usr/share/webapps/piwik";
      extraConfig = ''
        index index.php;
      '';
      locations."~ .php$".extraConfig = ''
        include ${pkgs.nginx}/conf/fastcgi_params;
        fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;
        fastcgi_pass 172.23.75.23:9000;
      '';
    };

    virtualHosts."piwik.higgsboson.tk" = {
      useACMEHost = "higgsboson.tk";
      forceSSL = true;
      globalRedirect = "piwik.thalheim.io";
    };
  };
}
