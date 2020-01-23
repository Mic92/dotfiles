{ pkgs, config, ... }:
{
  services.phpfpm.pools.adminer = {
    user = "adminer";
    group = "adminer";
    settings = {
      "listen.owner" = "nginx";
      "listen.group" = "nginx";
      "pm" = "dynamic";
      "pm.max_children" = 32;
      "pm.start_servers" = 1;
      "pm.min_spare_servers" = 1;
      "pm.max_spare_servers" = 1;
      "pm.max_requests" = 500;
    };
  };

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
        fastcgi_pass unix:${config.services.phpfpm.pools.adminer.socket};
        fastcgi_index index.php;
        fastcgi_param SCRIPT_FILENAME $document_root/$fastcgi_script_name;
      '';
      root = "${pkgs.nur.repos.mic92.adminer}/share/adminer";
    };

    virtualHosts."adminer.higgsboson.tk" = {
      useACMEHost = "higgsboson.tk";
      forceSSL = true;
      globalRedirect = "adminer.thalheim.io";
    };
  };

  users.users.adminer = {
    isSystemUser = true;
    createHome = true;
    group = "adminer";
  };

  users.groups.adminer = {};
  services.netdata.httpcheck.checks.adminer = {
    url = "https://adminer.thalheim.io";
    regex = "Login";
  };

  services.icinga2.extraConfig = ''
    apply Service "Adminer v4 (eve)" {
      import "eve-http4-service"
      vars.http_vhost = "adminer.thalheim.io"
      vars.http_uri = "/"
      assign where host.name == "eve.thalheim.io"
    }

    apply Service "Adminer v6 (eve)" {
      import "eve-http6-service"
      vars.http_vhost = "adminer.thalheim.io"
      vars.http_uri = "/"
      assign where host.name == "eve.thalheim.io"
    }
  '';
}
