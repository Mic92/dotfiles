{ pkgs, ... }:
{
  environment.systemPackages = [
    pkgs.sunshine
    pkgs.moonlight-qt # for testing purposes.
  ];
  services.udev.packages = [ pkgs.sunshine ];
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      47984
      47989
      47990
      48010
    ];
    allowedUDPPortRanges = [
      {
        from = 47998;
        to = 48000;
      }
      {
        from = 8000;
        to = 8010;
      }
    ];
  };
  # Prevents this error:
  # Fatal: You must run [sudo setcap cap_sys_admin+p $(readlink -f sunshine)] for KMS display capture to work!
  security.wrappers.sunshine = {
    owner = "root";
    group = "root";
    capabilities = "cap_sys_admin+p";
    source = "${pkgs.sunshine}/bin/sunshine";
  };
  # Needed for network discovery
  services.avahi.enable = true;
  services.avahi.publish.enable = true;
  services.avahi.publish.userServices = true;
}
