{
  services.xserver = {
    enable = true;
    desktopManager.xfce.enable = true;
    layout = "us";
    xkbVariant = "altgr-intl";
    xkbOptions = "caps:ctrl_modifier,compose:menu";
  };
}
