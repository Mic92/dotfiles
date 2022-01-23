{ pkgs, ... }: {
  services.xserver = {
    enable = true;
    libinput.enable = true;
    displayManager.gdm.enable = true;
    displayManager.defaultSession = "gnome";
    desktopManager.gnome.enable = true;
  };
  environment.systemPackages = [
    pkgs.gnome3.gnome-tweaks
    pkgs.gnome3.dconf-editor
    pkgs.gnomeExtensions.vitals
    pkgs.gnomeExtensions.forge
  ];
}
