{
  pkgs,
  config,
  self,
  ...
}:
{
  imports = [ ./kwallet-tpm ];
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
  # KWallet is unlocked via TPM (see ./kwallet-tpm), not PAM
  services.gnome.gnome-keyring.enable = false;

  # greetd auto-starts niri, noctalia-shell's lock screen handles login
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${config.programs.niri.package}/bin/niri-session";
        user = "joerg";
      };
    };
  };

  # PAM integration: noctalia-shell's lock screen uses the "login" PAM service
  # by default (auto-detected), so no extra PAM config is needed

  environment.systemPackages = with pkgs; [
    # Wayland clipboard & typing
    wl-clipboard
    cliphist
    wtype

    # Password manager
    rofi-rbw

    # App launcher, idle, display config
    fuzzel
    libnotify
    swayidle
    kanshi

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
    self.packages.${pkgs.hostPlatform.system}.live-text

    # Choosers (audio, display, internet)
    (pkgs.callPackage ../kde/choosers.nix { })
  ];

  programs.kdeconnect.enable = true;

  # Needed so xdg-open works properly with Nix-installed apps
  environment.sessionVariables = {
    NIX_PROFILES = "${pkgs.lib.concatStringsSep " " (
      pkgs.lib.reverseList config.environment.profiles
    )}";
    # Enable native Wayland support for Electron apps (Ferdium, etc.) and Chromium
    NIXOS_OZONE_WL = "1";
  };
}
