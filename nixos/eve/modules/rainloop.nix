{ pkgs, config, ... }: {
  services.phpfpm.pools.rainloop = {
    user = "rainloop";
    group = "rainloop";
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
    virtualHosts."mail.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
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
        fastcgi_pass unix:${config.services.phpfpm.pools.rainloop.socket};
      '';
      root = (pkgs.rainloop-community.override {
        dataPath = "/var/lib/rainloop";
      });
    };
  };

  services.netdata.httpcheck.checks.rainloop = {
    url = "https://mail.thalheim.io";
    regex = "javascript";
  };

  users.users.rainloop = {
    isSystemUser = true;
    createHome = true;
    home = "/var/lib/rainloop";
    group = "rainloop";
  };

  users.groups.rainloop = {};
}
