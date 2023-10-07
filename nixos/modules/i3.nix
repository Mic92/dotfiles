{ pkgs, ... }: {
  imports = [
    ./xserver.nix
    ./flameshot.nix
  ];
  services.xserver.windowManager.i3.enable = true;
  services.xserver.displayManager.defaultSession = "none+i3";

  services.dbus.packages = [ pkgs.flameshot ];
  environment.systemPackages = with pkgs; [
    flameshot
    xorg.xmodmap
    firefox
    chromium
    # autostart stuff
    dex
    brightnessctl
    pavucontrol
    lightlocker
    lxappearance
    scrot
    evince
    rofi
    gnome.eog
    libnotify
    dunst
    pamixer
    mpc_cli
    clipit
    picom
    xclip
    xorg.xev
    xorg.xprop
    alacritty
    (i3pystatus.override {
      extraLibs = with python3.pkgs; [ keyrings-alt paho-mqtt ];
    })
    networkmanagerapplet
    gnome.file-roller
    gnome.nautilus
  ];

  services.gvfs.enable = true;

  services.autorandr.enable = true;
  programs.nm-applet.enable = true;
}
