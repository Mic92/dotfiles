{ config, pkgs, ... }: 
let
  monitorPool = poolName: virtualHost: {...}: {
    services.phpfpm.pools.${poolName} = {
      extraConfig = ''
        pm.status_path = /_status
      '';
    };
    services.nginx.virtualHosts.${virtualHost} = {
      locations."~ ^/_status$".extraConfig = ''
        access_log off;
        allow 127.0.0.1;
        allow ::1;
        allow ${config.networking.eve.ipv4.address};
        allow ${config.networking.eve.ipv6.subnet};
        deny all;
        include ${pkgs.nginx}/conf/fastcgi_params;
        fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;
        fastcgi_index index.php;
        fastcgi_pass unix:${config.services.phpfpm.pools.${poolName}.socket};
      '';
    };

    environment.etc."netdata/python.d/phpfpm.conf".text = ''
      ${poolName}:
        name: '${poolName}'
        url: "https://${virtualHost}/_status?full&json"
    '';
  };
in {
  imports = [
    (monitorPool "mediawiki" "ist.devkid.net")
    (monitorPool "rainloop" "mail.thalheim.io")
    (monitorPool "adminer" "adminer.thalheim.io")
    (monitorPool "phpldapadmin" "ldap.thalheim.io")
    (monitorPool "nextcloud" "cloud.thalheim.io")
    (monitorPool "tt-rss" "rss.devkid.net")
  ];
}
