{
  pkgs,
  config,
  lib,
  ...
}: let
  maxUploadSize = "256M";
  toKeyValue = lib.generators.toKeyValue {
    mkKeyValue = lib.generators.mkKeyValueDefault {} " = ";
  };
in {
  services.phpfpm.pools.rainloop = {
    user = "rainloop";
    group = "rainloop";
    phpOptions = toKeyValue {
      upload_max_filesize = maxUploadSize;
      post_max_size = maxUploadSize;
      memory_limit = maxUploadSize;
    };

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
      extraConfig = ''
        client_max_body_size ${maxUploadSize};
      '';
      root = pkgs.rainloop-community.override {
        dataPath = "/var/lib/rainloop";
      };
    };
  };

  users.users.rainloop = {
    isSystemUser = true;
    createHome = true;
    home = "/var/lib/rainloop";
    group = "rainloop";
  };

  users.groups.rainloop = {};
}
