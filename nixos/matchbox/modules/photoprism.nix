{ config, ... }:
{
  sops.secrets."photoprism-password" = {};
  services.photoprism = {
    enable = true;
    address = "192.168.178.2";
    originalsPath = "/mnt/hdd/public/Bilder";
    passwordFile = config.sops.secrets."photoprism-password".path;
  };

  fileSystems."/var/lib/private/photoprism" = {
    device = "/mnt/hdd/photoprism";
    options = [ "bind" "nofail" ];
  };

  networking.firewall.allowedTCPPorts = [ 2342 ];
}
