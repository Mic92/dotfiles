{ pkgs, ... }: {
  services.xserver = {
    enable = true;
    libinput.enable = true;
    displayManager.gdm.enable = true;
    displayManager.defaultSession = "gnome";
    desktopManager.gnome.enable = true;
  };
  programs.kdeconnect = {
    package = pkgs.gnomeExtensions.gsconnect;
    enable = true;
  };
  environment.systemPackages = [
    pkgs.playerctl # gsconnect play/pause command
    pkgs.pamixer # gcsconnect volume control
    pkgs.gnome.gnome-tweaks
    pkgs.gnome.dconf-editor
    pkgs.gnomeExtensions.vitals
    pkgs.gnomeExtensions.forge
  ];
}
