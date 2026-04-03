{
  pkgs,
  config,
  self,
  ...
}:
{
  imports = [
    ./kwallet-tpm
    ./janet.nix
  ];

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

    # App launcher, display config
    fuzzel
    libnotify
    kanshi
    wdisplays # drag-and-drop output arrangement; launched from noctalia display-config plugin for 3+ monitor setups

    # Audio
    pavucontrol

    # Desktop shell (bar, notifications, control center, OSD)
    noctalia-shell
    # Qt's wayland QPA leaves QIcon::themeName empty so noctalia falls through
    # to hicolor and can't find generic icons like user-desktop. The gtk3
    # platform theme reads gtk-icon-theme-name; ship breeze so that resolves.
    kdePackages.breeze-icons

    # Needed by noctalia kde-connect plugin's "Browse files" (SFTP mount)
    sshfs

    # Browsers / apps
    chromium
    ferdium

    # File manager — terminal-based, themed via noctalia's yazi template
    # (wallpaper-derived colors). GUI file manager dropped; xdg-open on
    # directories falls back to yazi via its .desktop file.
    yazi

    # Document viewer
    evince

    # Image viewer — xdg-open target for the noctalia nostr-chat
    # plugin's tap-to-zoom on decrypted attachments.
    eog

    # Printer configuration
    system-config-printer

    # KDE Wallet (secret service provider + management UI)
    kdePackages.kwallet
    kdePackages.kwalletmanager

    # Screenshot tools
    grim
    slurp

    # live-text: OCR overlay, screenshot annotation, region capture
    self.packages.${pkgs.stdenv.hostPlatform.system}.live-text
  ];

  programs.kdeconnect.enable = true;

  # Needed so xdg-open works properly with Nix-installed apps
  environment.sessionVariables = {
    NIX_PROFILES = "${pkgs.lib.concatStringsSep " " (
      pkgs.lib.reverseList config.environment.profiles
    )}";
    # Enable native Wayland support for Electron apps (Ferdium, etc.) and Chromium
    NIXOS_OZONE_WL = "1";
    # Make Qt resolve icon themes via GTK settings instead of defaulting to
    # hicolor-only on the wayland QPA (see breeze-icons above).
    QT_QPA_PLATFORMTHEME = "gtk3";
  };
}
