{ config, ... }: {
  sops.secrets.photoprism = { };
  services.photoprism = {
    enable = true;
    address = "::";
    settings.PHOTOPRISM_AUTH_MODE = "password";
    originalsPath = "/zdata/photos";
    passwordFile = config.sops.secrets.photoprism.path;
  };

  systemd.tmpfiles.rules = [
    "d /zdata/photos 0755 photoprism photoprism -"
    "d /zdata/photoprism 0755 photoprism photoprism -"
  ];

  fileSystems."/var/lib/private/photoprism" = {
    device = "/zdata/photoprism";
    options = [ "bind" "nofail" ];
  };

  networking.firewall.allowedTCPPorts = [ 2342 ];
}
