{ pkgs, ... }: {
  services.nginx = {
    virtualHosts."pyload.devkid.net" = {
      useACMEHost = "devkid.net";
      forceSSL = true;
      locations."/".extraConfig = ''
        fastcgi_pass 172.23.75.26:8001;
      '';
    };
  };
}
