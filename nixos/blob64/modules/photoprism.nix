{ config, ... }: {
  services.photoprism = {
    enable = true;
    address = "[::]";
    settings.PHOTOPRISM_AUTH_MODE = "password";
    settings.PHOTOPRISM_UPLOAD_NSFW = "true";
    settings.PHOTOPRISM_DETECT_NSFW = "false";
    settings.PHOTOPRISM_SITE_URL = "https://photoprism.thalheim.io";
    originalsPath = "/zdata/photos";
    passwordFile = config.sops.secrets.photoprism.path;
  };
  systemd.services.photoprism.serviceConfig.ReadWritePaths = "/zdata/photos";

  systemd.tmpfiles.rules = [
    "d /zdata/photos 0755 photoprism photoprism -"
    "d /zdata/photoprism 0755 photoprism photoprism -"
  ];

  fileSystems."/var/lib/private/photoprism" = {
    device = "/zdata/photoprism";
    options = [ "bind" "nofail" ];
  };

  systemd.services.photoprism = {
    unitConfig.RequiresMountsFor = "/var/lib/private/photoprism";
  };

  networking.firewall.allowedTCPPorts = [ 2342 ];
}
