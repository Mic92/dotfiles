{ pkgs, lib, ... }: {
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true; # so that gtk works properly
    extraPackages = with pkgs; [
      dex
      swaylock # lockscreen
      pavucontrol
      swayidle
      xwayland
      waybar # status bar
      (i3pystatus.override {
        extraLibs = [ python3.pkgs.keyrings-alt python3.pkgs.paho-mqtt ];
      })
      wofi
      gnome3.eog
      libnotify
      mako # notification daemon
      kanshi # autorandr
      swappy
      wdisplays
      wl-clipboard
      clipman

      pavucontrol
      lightlocker
      evince
      libnotify
      pamixer
      mpc_cli
      gnome3.networkmanagerapplet
      gnome3.file-roller
      gnome3.nautilus
      firefox-wayland
    ];
  };

  environment.sessionVariables = {
    MOZ_ENABLE_WAYLAND = "1";
    XDG_CURRENT_DESKTOP = "sway";
    SDL_VIDEODRIVER = "wayland";
    QT_QPA_PLATFORM = "wayland";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
    _JAVA_AWT_WM_NONREPARENTING = "1";
  };

  environment.systemPackages = with pkgs; [
    # polkit agent
    polkit_gnome

    # gtk3 themes
    gtk-engine-murrine
    gtk_engines
    gsettings-desktop-schemas
    lxappearance

    # screen brightness
    brightnessctl

    # Here we but a shell script into path, which lets us start sway.service (after importing the environment of the login shell).
    (pkgs.writeTextFile {
      name = "startsway";
      destination = "/bin/startsway";
      executable = true;
      text = ''
        #! ${pkgs.bash}/bin/bash

        # first import environment variables from the login manager
        systemctl --user import-environment
        # then start the service
        exec systemctl --user start sway.service
      '';
    })
  ];

  # for polkit
  environment.pathsToLink = [ "/libexec" ];

  programs.qt5ct.enable = true;

  # brightnessctl
  users.users.joerg.extraGroups = [ "video" ];

  systemd.user.targets.sway-session = {
    description = "Sway compositor session";
    documentation = [ "man:systemd.special(7)" ];
    bindsTo = [ "graphical-session.target" ];
    wants = [ "graphical-session-pre.target" ];
    after = [ "graphical-session-pre.target" ];
  };

  systemd.user.services.sway = {
    description = "Sway - Wayland window manager";
    documentation = [ "man:sway(5)" ];
    bindsTo = [ "graphical-session.target" ];
    wants = [ "graphical-session-pre.target" ];
    after = [ "graphical-session-pre.target" ];
    # We explicitly unset PATH here, as we want it to be set by
    # systemctl --user import-environment in startsway
    environment.PATH = lib.mkForce null;
    serviceConfig = {
      Type = "simple";
      ExecStart = ''
        ${pkgs.dbus}/bin/dbus-run-session ${pkgs.sway}/bin/sway --debug
      '';
      Restart = "on-failure";
      RestartSec = 1;
      TimeoutStopSec = 10;
    };
  };
}
