{
  ...
}:
let
  downloadsDir = "/zdata/pinchflat";
in
{
  services.pinchflat = {
    enable = true;
    port = 8945;
    mediaDir = downloadsDir;
    openFirewall = false;
    selfhosted = true;
    extraConfig = {
      TZ = "Europe/Berlin";
      EXPOSE_FEED_ENDPOINTS = "true";
      ENABLE_IPV6 = "true";
    };
  };

  systemd.tmpfiles.rules = [
    "d ${downloadsDir} 0755 pinchflat pinchflat -"
    "d /zdata/pinchflat-state 0755 pinchflat pinchflat -"
  ];

  fileSystems."/var/lib/private/pinchflat" = {
    device = "/zdata/pinchflat-state";
    options = [
      "bind"
      "nofail"
    ];
  };

  systemd.services.pinchflat = {
    unitConfig.RequiresMountsFor = "/var/lib/private/pinchflat";
  };

  # Only allow access from wireguard (eve reverse proxy)
  networking.firewall.interfaces.wireguard.allowedTCPPorts = [ 8945 ];
}
