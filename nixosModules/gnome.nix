{ pkgs, ... }:
{
  services.xserver = {
    enable = true;
    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;
  };
  services.displayManager.defaultSession = "gnome";

  services.libinput.enable = true;
  programs.kdeconnect = {
    package = pkgs.gnomeExtensions.gsconnect;
    enable = true;
  };
  environment.systemPackages = [
    pkgs.playerctl # gsconnect play/pause command
    pkgs.pamixer # gcsconnect volume control
    pkgs.gnome-tweaks
    pkgs.dconf-editor
    pkgs.gnomeExtensions.vitals
    pkgs.gnomeExtensions.forge
  ];
}
