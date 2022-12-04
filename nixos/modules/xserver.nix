{
  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "altgr-intl";
    xkbOptions = "caps:escape,compose:menu";
    libinput.enable = true;
  };
}
