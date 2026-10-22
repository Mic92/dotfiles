{ pkgs, ... }:
let
  toml = pkgs.formats.toml { };

  # Pinned instead of noctalia's own git sources so plugin updates go
  # through flake bumps like everything else.
  official = pkgs.fetchFromGitHub {
    owner = "noctalia-dev";
    repo = "official-plugins";
    rev = "10dd0255e45297e5d8a41c78f08387a7bbd21b1c";
    hash = "sha256-a4hRxpo88vn4G5I7IE4WW9JpSxD3Bw2daBI5C+DkT4c=";
  };
  community = pkgs.fetchFromGitHub {
    owner = "noctalia-dev";
    repo = "community-plugins";
    rev = "f1f51da23a2867e6447650a9fca5d671fc30bec8";
    hash = "sha256-z9ks63WDvH2a2lDmj2ey5y++e1E+HQnsv0dNVUOfPyk=";
  };

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
