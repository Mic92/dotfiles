{ pkgs
, config
, ...
}: {
  services.phpfpm.pools.adminer = {
    user = "adminer";
    group = "adminer";
    settings = {
      "listen.owner" = "nginx";
      "listen.group" = "nginx";
      "pm" = "ondemand";
      "pm.max_children" = 32;
      "pm.process_idle_timeout" = "10s";
      "pm.max_requests" = 500;
    };
  };

  services.nginx = {
    virtualHosts."adminer.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      extraConfig = ''
        index adminer.php;
      '';
      locations."/".extraConfig = ''
        try_files $uri $uri/ /index.php?$args;
      '';
      locations."~* ^.+\.php$".extraConfig = ''
        include ${pkgs.nginx}/conf/fastcgi_params;
        fastcgi_pass unix:${config.services.phpfpm.pools.adminer.socket};
        fastcgi_index adminer.php;
        fastcgi_param SCRIPT_FILENAME $document_root/$fastcgi_script_name;
      '';
      root = "${pkgs.adminer}";
    };
  };

  users.users.adminer = {
    isSystemUser = true;
    createHome = true;
    group = "adminer";
  };

  users.groups.adminer = { };
}
