{ config, ... }:
{
  services.photoprism = {
    enable = true;
    address = "192.168.178.2";
    port = 80;
    originalsPath = "/mnt/hdd/public/Bilder";
    settings.PHOTOPRISM_AUTH_MODE = "public";
  };

  fileSystems."/var/lib/private/photoprism" = {
    device = "/mnt/hdd/photoprism";
    options = [ "bind" "nofail" ];
  };

  networking.firewall.allowedTCPPorts = [ 2342 ];
}
