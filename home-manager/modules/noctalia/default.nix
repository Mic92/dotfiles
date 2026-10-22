{ pkgs, ... }:
let
  toml = pkgs.formats.toml { };

  # Pinned instead of noctalia's own git sources so plugin updates go
  # through dotfiles commits like everything else.
  inherit (pkgs.callPackage ./plugin-sources.nix { }) official community;

  # Point at the repo, not homeshick's per-file symlinks, so inotify
  # hot-reload sees edits.
  repo = "~/.homesick/repos/dotfiles/home/.config/noctalia";

  audio-switch = pkgs.writeShellApplication {
    name = "audio-switch";
    runtimeInputs = with pkgs; [
      bluez
      libnotify
      pamixer
      pulseaudio
    ];
    text = builtins.readFile ./audio-switch.sh;
  };
  net-switch = pkgs.writeShellApplication {
    name = "net-switch";
    runtimeInputs = with pkgs; [
      libnotify
      networkmanager
    ];
    text = builtins.readFile ./net-switch.sh;
  };

  # dmenu `exec` substitutes {selection} verbatim into a shell string.
  dmenu = bin: {
    command = "${bin} list";
    exec = "${bin} '{selection}'";
  };
in
{
  xdg.configFile."noctalia/config.toml".source = ./config.toml;
  xdg.configFile."noctalia/nix.toml".source = toml.generate "noctalia-nix.toml" {
    plugins = {
      auto_update = "none";
      enabled = [
        "noctalia/timer"
        "raycursive/niri-displays"
        "nightwatch75/file-search"
        "icefish/phone-connect"
        "aristides/udiskie"
        "mic92/ssh-askpass"
        "mic92/fprint-notify"
        "mic92/alertmanager"
        "mic92/mail-count"
        "mic92/rbw-provider"
      ];
      source = [
        {
          name = "official";
          kind = "git";
          location = "https://github.com/noctalia-dev/official-plugins";
          enabled = false;
        }
        {
          name = "community";
          kind = "git";
          location = "https://github.com/noctalia-dev/community-plugins";
          enabled = false;
        }
        {
          name = "official-pinned";
          kind = "path";
          location = "${official}";
        }
        {
          name = "community-pinned";
          kind = "path";
          location = "${community}";
        }
        {
          name = "mic92";
          kind = "path";
          location = "${repo}/shared-plugins";
        }
        {
          name = "local";
          kind = "path";
          location = "${repo}/local-plugins";
        }
      ];
    };
    shell.launcher.dmenu.entry = {
      audio = dmenu "${audio-switch}/bin/audio-switch" // {
        prefix = "audio";
        glyph = "volume";
      };
      net = dmenu "${net-switch}/bin/net-switch" // {
        prefix = "net";
        glyph = "world";
      };
    };
  };
}
