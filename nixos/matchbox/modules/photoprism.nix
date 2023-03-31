{ config, ... }:
{
  services.photoprism = {
    enable = true;
    originalsPath = "/mnt/hdd/public/Bilder";
    settings.PHOTOPRISM_AUTH_MODE = "public";
  };

  services.nginx = {
    enable = true;
    virtualHosts."192.168.178.2".locations."/".proxyPass = "http://localhost:2342";
  };

  fileSystems."/var/lib/private/photoprism" = {
    device = "/mnt/hdd/photoprism";
    options = [ "bind" "nofail" ];
  };

  networking.firewall.allowedTCPPorts = [ 80 ];
}
