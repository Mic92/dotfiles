{
  pkgs,
  config,
  self,
  sshAskPasswordWrapper,
  ...
}:
{
  imports = [
    ./kwallet-tpm
    ./janet.nix
  ];

  # programs.ssh.enableAskPassword defaults to services.xserver.enable, which
  # is false under niri → NixOS exports SSH_ASKPASS="" instead of leaving it
  # unset. Force-enable and point at the noctalia-aware wrapper so ssh-add
  # invoked from a shell uses the same dialog as agent-side confirmations.
  # KDE machines keep ksshaskpass (set by the plasma6 nixos module).
  programs.ssh.enableAskPassword = true;
  programs.ssh.askPassword = "${sshAskPasswordWrapper}";

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

  # noctalia-shell as a supervised service so quickshell crashes (segfaults in
  # the QML engine, GPU resets, OOM kills) don't leave the session without a
  # bar/launcher/lock-screen. spawn-at-startup + systemd-run --scope gave us
  # cgroup isolation but no restart; a real unit gets both.
  systemd.user.services.noctalia-shell = {
    description = "Noctalia desktop shell (quickshell)";
    partOf = [ "graphical-session.target" ];
    after = [ "graphical-session.target" ];
    requisite = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];
    # QML shells out via `sh -c` for plugin discovery and plugin commands
    # (fc-list, df, notmuch, khal, rbw, ...). Setting `path` emits
    # Environment=PATH=... which *replaces* the user manager's imported PATH,
    # and __NIXOS_SET_ENVIRONMENT_DONE blocks `sh -lc` from recovering it via
    # /etc/profile. So pin the hard deps *and* re-add the standard profile dirs
    # (%h/%u are systemd specifiers, they pass through makeBinPath verbatim).
    path = with pkgs; [
      bash
      git
      fontconfig
      procps
      "/run/wrappers"
      "%h/.nix-profile"
      "/etc/profiles/per-user/%u"
      "/nix/var/nix/profiles/default"
      "/run/current-system/sw"
    ];
    # Back off if it crash-loops on a broken config instead of pegging a core.
    startLimitIntervalSec = 30;
    startLimitBurst = 5;
    serviceConfig = {
      ExecStart = "${pkgs.noctalia-shell}/bin/noctalia-shell";
      Restart = "always";
      RestartSec = 1;
      Slice = "app-graphical.slice";
    };
  };

  # noctalia-shell rewrites plugins.json via atomic rename, which clobbers the
  # homeshick symlink with a plain file. Watch for that and fold the new
  # content back into the dotfiles repo, then restore the link — so plugin
  # toggles made in the UI still end up under version control.
  systemd.user.paths.noctalia-plugins-relink = {
    wantedBy = [ "default.target" ];
    pathConfig.PathChanged = "%h/.config/noctalia/plugins.json";
  };
  systemd.user.services.noctalia-plugins-relink = {
    serviceConfig = {
      Type = "oneshot";
      ExecStart = pkgs.writeShellScript "noctalia-plugins-relink" ''
        set -eu
        live="$HOME/.config/noctalia/plugins.json"
        repo="$HOME/.homesick/repos/dotfiles/home/.config/noctalia/plugins.json"
        # Nothing to do if it's still (or already) a symlink.
        [ -L "$live" ] && exit 0
        [ -f "$live" ] || exit 0
        mv -f "$live" "$repo"
        ln -sf ../../.homesick/repos/dotfiles/home/.config/noctalia/plugins.json "$live"
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
    # to hicolor and can't find generic icons like user-desktop. qt6ct reads
    # icon_theme from its conf; ship breeze so that resolves.
    kdePackages.breeze-icons

    # GTK theme that respects ~/.config/gtk-{3,4}.0/gtk.css colour overrides
    # generated by noctalia, so GTK and Electron apps follow the palette.
    adw-gtk3
    bibata-cursors

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
    XCURSOR_THEME = "Bibata-Modern-Classic";
    XCURSOR_SIZE = "24";
  };

  # Ships qt5ct + qt6ct platform plugins and sets QT_QPA_PLATFORMTHEME so Qt
  # apps pick up the noctalia-generated colour scheme at
  # ~/.config/qt{5,6}ct/colors/noctalia.conf.
  qt = {
    enable = true;
    platformTheme = "qt5ct";
  };
}
