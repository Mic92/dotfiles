{ pkgs, ... }:
let
  page = acmeHost: {
    useACMEHost = acmeHost;
    forceSSL = true;

    extraConfig = ''
      fastcgi_buffers 64 4K;
      
      rewrite ^/caldav(.*)$ /remote.php/caldav$1 redirect;
      rewrite ^/carddav(.*)$ /remote.php/carddav$1 redirect;
      rewrite ^/webdav(.*)$ /remote.php/webdav$1 redirect;
      
      index index.php;
      error_page 403 /core/templates/403.php;
      error_page 404 /core/templates/404.php;
      add_header X-Content-Type-Options nosniff;
      disable_symlinks off;
    '';

    locations."~ ^/(data|config|\.ht|db_structure\.xml|README)".extraConfig = ''
      deny all;
    '';
    locations."/".extraConfig = ''
      # The following 2 rules are only needed with webfinger
      rewrite ^/.well-known/host-meta /public.php?service=host-meta last;
      rewrite ^/.well-known/host-meta.json /public.php?service=host-meta-json last;
      
      rewrite ^/.well-known/carddav /remote.php/carddav/ redirect;
      rewrite ^/.well-known/caldav /remote.php/caldav/ redirect;
      
      rewrite ^(/core/doc/[^\/]+/)$ $1/index.html;
      
      try_files $uri $uri/ index.php;
    '';
    locations."~ ^(.+?\.php)(/.*)?$".extraConfig = ''
      try_files $1 = 404;
      
      include ${pkgs.nginx}/conf/fastcgi_params;
      fastcgi_param   SCRIPT_FILENAME $document_root$fastcgi_script_name;
      fastcgi_param   PATH_INFO $2;
      fastcgi_index   index.php;
      fastcgi_pass    172.23.75.15:9000;
    '';
    root = "/usr/share/webapps/nextcloud";
  };
in {
  services.nginx = {
    virtualHosts."cloud.thalheim.io" = page "thalheim.io";
    virtualHosts."pim.devkid.net" = page "devkid.net";

    virtualHosts."cloud.higgsboson.tk" = {
      useACMEHost = "higgsboson.tk";
      forceSSL = true;
      globalRedirect = "cloud.thalheim.io";
    };
  };
}
