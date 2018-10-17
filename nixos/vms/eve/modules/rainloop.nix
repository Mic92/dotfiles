{ pkgs, ... }: {
  services.phpfpm.pools.rainloop = {
    listen = "/run/phpfpm-rainloop.sock";
    extraConfig = ''
      listen.owner = nginx
      listen.group = nginx
      user = rainloop
      group = rainloop
      pm = dynamic
      pm.max_children = 32
      pm.start_servers = 2
      pm.min_spare_servers = 1
      pm.max_spare_servers = 2
      pm.max_requests = 500
    '';
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
        fastcgi_pass unix:/run/phpfpm-rainloop.sock;
      '';
      root = (pkgs.rainloop-community.override {
        dataPath = "/var/lib/rainloop";
      });
    };

    virtualHosts."mail.higgsboson.tk" = {
      useACMEHost = "higgsboson.tk";
      forceSSL = true;
      globalRedirect = "mail.thalheim.io";
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
