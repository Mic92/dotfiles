{ config, ... }:
let
  site = root: {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    root = "/var/www/${root}";
    locations."/files/".extraConfig = ''
      internal;
      secure_link $arg_st;
      include ${config.sops.secrets.nginx-secure-link.path};

      if ($secure_link = "") { return 403; }
      if ($secure_link = "0") { return 403; }
    '';
    locations."/".extraConfig = ''
      rewrite /([a-zA-Z0-9_\-]*)/(.*)$ /$2?st=$1;
    '';
  };
in
{
  services.nginx = {
    virtualHosts."dl.devkid.net" = site "dl.devkid.net";
    virtualHosts."dl.thalheim.io" = site "dl.thalheim.io";
    virtualHosts."dl.lekwati.com" = site "dl.lekwati.com";
  };

  sops.secrets.nginx-secure-link.owner = "nginx";
}
