{ pkgs, ... }:
{
  services.nginx = {
    virtualHosts."ist.devkid.net" = {
      useACMEHost = "devkid.net";
      forceSSL = true;
      locations."= /robots.txt".extraConfig = ''
        access_log off; log_not_found off;
      '';
      locations."= /facicon.ico".extraConfig = ''
        access_log off; log_not_found off;
      '';
      locations."/".extraConfig = ''
        try_files $uri $uri/ @rewrite;
      '';
      locations."@rewrite".extraConfig = ''
        rewrite ^/wiki([^?]*)(?:\?(.*))? /index.php?title=$1&$2 last;
      '';
      locations."~ \.(php|php5)$".extraConfig = ''
        include ${pkgs.nginx}/conf/fastcgi_params;
        fastcgi_param   SCRIPT_FILENAME $document_root$fastcgi_script_name;
        fastcgi_index   index.php;
        fastcgi_pass    172.23.75.17:9000;
      '';
      extraConfig = ''
        add_header X-Frame-Options DENY;
 
        index index.php index.html index.htm;

        # anti spam
        rewrite ^/richtige-anmeldung.php$ /index.php?title=Spezial:Anmelden&type=signup&spam=nospam;
        if ($arg_title = Spezial:Anmelden) { set $rewritecond "1"; }
        if ($arg_type = signup) { set $rewritecond "''${rewritecond}2"; }
        if ($arg_spam != nospam) { set $rewritecond "''${rewritecond}3"; }
        if ($arg_action != submitlogin) { set $rewritecond "''${rewritecond}4"; }
        if ($rewritecond = 1234) { rewrite ^ /anmeldung.php last; }
      '';
      root = "/srv/http/ist.devkid.net";
    };

    virtualHosts."www.ist.devkid.net" = {
      useACMEHost = "devkid.net";
      forceSSL = true;
      globalRedirect = "ist.devkid.net";
    };
  };
}
