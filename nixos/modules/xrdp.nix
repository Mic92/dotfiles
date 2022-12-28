{
  networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [ 3389 ];
  services.xrdp.enable = true;
  services.xrdp.defaultWindowManager = "xfce4-session";
  services.xserver = {
    enable = true;
    desktopManager.xfce.enable = true;
    layout = "us";
    xkbVariant = "altgr-intl";
  };
}
