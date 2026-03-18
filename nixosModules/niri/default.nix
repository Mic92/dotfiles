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
        # Lock KWallet/ksecretd
        ${pkgs.libsecret}/bin/secret-tool lock --collection=kdewallet 2>/dev/null || true

        # Lock rbw
        ${pkgs.rbw}/bin/rbw lock 2>/dev/null || true
      '';
    };
  };

  programs.niri.enable = true;

  # Use KDE Wallet instead of gnome-keyring for secret storage
  services.gnome.gnome-keyring.enable = false;
  security.pam.services.greetd.kwallet = {
    enable = true;
    package = pkgs.kdePackages.kwallet-pam;
    forceRun = true;
  };

  # greetd + tuigreet as display manager (lightweight, no X dependency)
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --remember --remember-session --sessions ${config.services.displayManager.sessionData.desktops}/share/wayland-sessions";
        user = "greeter";
      };
    };
  };

  # PAM integration for swaylock is handled by wayland-session.nix
  # imported by the niri module

  environment.systemPackages = with pkgs; [
    # Wayland clipboard & typing
    wl-clipboard
    cliphist
    wtype

    # Password manager
    rofi-rbw

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

    # File manager
    nautilus

    # Document viewer
    evince

    # Printer configuration
    system-config-printer

    # KDE Wallet (secret service provider + management UI)
    kdePackages.kwallet
    kdePackages.kwalletmanager

    # Screenshot tools
    grim
    slurp

    # live-text: OCR overlay, screenshot annotation, region capture
    (pkgs.python3.pkgs.callPackage ../../pkgs/live-text { })

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
