{
  services.photoprism = {
    enable = true;
    originalsPath = "/mnt/hdd/public/Bilder";
    settings.PHOTOPRISM_AUTH_MODE = "public";
    settings.PHOTOPRISM_UPLOAD_NSFW = "true";
    settings.PHOTOPRISM_DETECT_NSFW = "false";
  };

  services.nginx = {
    enable = true;
    virtualHosts."192.168.178.2" = {
      locations."/".proxyPass = "http://localhost:2342";
      locations."/".proxyWebsockets = true;
      listenAddresses = [ "192.168.178.2" ];
    };
  };

  fileSystems."/var/lib/private/photoprism" = {
    device = "/mnt/hdd/photoprism";
    options = [ "bind" "nofail" ];
  };
  systemd.services.photoprism = {
    unitConfig.RequiresMountsFor = "/var/lib/private/photoprism";
  };

  networking.firewall.allowedTCPPorts = [ 80 ];
}
