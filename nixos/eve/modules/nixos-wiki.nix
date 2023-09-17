{ config, ... }: {
  services.mediawiki.enable = true;
  services.mediawiki.webserver = "none";
  services.mediawiki.database.type = "postgres";
  services.mediawiki.url = "https://nixos-wiki.thalheim.io";
  services.mediawiki.passwordFile = config.sops.secrets."nixos-wiki".path;

  services.mediawiki.poolConfig = {
    "listen.owner" = "nginx";
    "listen.group" = "nginx";
    "pm" = "ondemand";
    "pm.max_children" = 32;
    "pm.max_requests" = 500;
    "pm.process_idle_timeout" = "10s";
  };

  sops.secrets."nixos-wiki".owner = config.services.phpfpm.pools.mediawiki.user;

  services.nginx.virtualHosts."nixos-wiki.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    root = "${config.services.mediawiki.package}/share/mediawiki";
    extraConfig = ''
      index index.php index.html index.htm;
    '';
    locations = {
      "/".tryFiles = "$uri $uri/ @rewrite";
      "/".index = "index.php";
      "@rewrite".extraConfig = "rewrite ^/(.*)$ /index.php?title=$1&$args;";
      "~ /maintenance/".extraConfig = "return 403;";
      "/rest.php".tryFiles = "$uri $uri/ /rest.php?$args";

      "~ \\.php$".extraConfig = ''
        include ${config.services.nginx.package}/conf/fastcgi_params;
        fastcgi_index index.php;
        fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;
        fastcgi_pass unix:${config.services.phpfpm.pools.mediawiki.socket};
      '';

      "~* \\.(js|css|png|jpg|jpeg|gif|ico)$".extraConfig = ''
        try_files $uri /index.php;
        expires max;
        log_not_found off;
      '';

      "/images".extraConfig = ''
        root /var/lib/mediawiki/uploads;
      '';

      "/uploads".extraConfig = ''
        root /var/lib/mediawiki/uploads;
      '';

      "= /_.gif".extraConfig = ''
        expires max;
        empty_gif;
      '';

      "^~ /cache/".extraConfig = "deny all;";

      #"/dumps" = {
      #  root = "/var/www/mediawiki/local";
      #  extraConfig = "autoindex on;";
      #};
    };
  };
}
