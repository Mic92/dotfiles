{ pkgs, ... }: {
  services.xserver = {
    enable = true;
    libinput.enable = true;
    displayManager.gdm.enable = true;
    displayManager.defaultSession = "gnome";
    desktopManager.gnome.enable = true;
  };
  environment.systemPackages = [
    pkgs.gnome.gnome-tweaks
    pkgs.gnome.dconf-editor
    pkgs.gnomeExtensions.vitals
    pkgs.gnomeExtensions.forge
  ];
}
