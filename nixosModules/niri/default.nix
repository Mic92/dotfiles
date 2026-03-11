{
  pkgs,
  config,
  ...
}:
{
  # Lock secrets before suspend (carried over from KDE module)
  systemd.user.services.lock-secrets-on-suspend = {
    description = "Lock secrets before suspend";
    before = [ "sleep.target" ];
    wantedBy = [ "sleep.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = pkgs.writeShellScript "lock-secrets" ''
        ${pkgs.rbw}/bin/rbw lock 2>/dev/null || true
      '';
    };
  };

  programs.niri.enable = true;

  # SDDM as display manager
  services.xserver.enable = true;
  services.displayManager.sddm.enable = true;
  services.displayManager.sddm.wayland.enable = true;

  # PAM integration for swaylock is handled by wayland-session.nix
  # imported by the niri module

  environment.systemPackages = with pkgs; [
    # Wayland clipboard
    wl-clipboard

    # App launcher, screen lock, idle, display config
    fuzzel
    libnotify
    swaylock
    swayidle
    kanshi
    brightnessctl

    # Audio
    pavucontrol

    # Desktop shell (bar, notifications, control center, OSD)
    noctalia-shell

    # Browsers / apps
    chromium
    ferdium

    # Choosers (audio, display, internet)
    (pkgs.callPackage ../kde/choosers.nix { })
  ];

  programs.kdeconnect.enable = true;

  # Needed so xdg-open works properly with Nix-installed apps
  environment.sessionVariables = {
    NIX_PROFILES = "${pkgs.lib.concatStringsSep " " (
      pkgs.lib.reverseList config.environment.profiles
    )}";
  };
}
