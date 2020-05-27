{ pkgs, ... }: {
  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome3.enable = true;
  environment.systemPackages = [
    pkgs.gnome3.gnome-tweaks
    pkgs.gnome3.dconf-editor
  ];
}
