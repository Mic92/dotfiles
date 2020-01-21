{ pkgs, ...}: 
{
  services.nginx = {
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
      locations."/_status".extraConfig = ''
        stub_status;
      '';
      extraConfig = ''
        # TODO
        #location /privat.html {
        #  auth_ldap "Forbidden";
        #  auth_ldap_servers default;
        #}
      '';
    };


    virtualHosts."www.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      globalRedirect = "thalheim.io";
    };

    virtualHosts."www.higgsboson.tk" = {
      useACMEHost = "higgsboson.tk";
      forceSSL = true;
      globalRedirect = "thalheim.io";
    };

    virtualHosts."higgsboson.tk" = {
      useACMEHost = "higgsboson.tk";
      forceSSL = true;
      globalRedirect = "thalheim.io";
    };
  };

  services.netdata.httpcheck.checks.homepage = {
    url = "https://thalheim.io";
    regex = "Higgs-Boson";
  };
}
