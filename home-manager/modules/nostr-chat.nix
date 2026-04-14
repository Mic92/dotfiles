# Darwin counterpart of noctalia-plugins' nixosModules.nostr-chat.
#
# Same daemon, same env-var contract — only the supervisor differs:
# launchd agents instead of a systemd user unit, plus a native menubar
# frontend in place of the QML panel. Values mirror
# nixosModules/niri/janet.nix so both machines DM the same peer.
{
  config,
  pkgs,
  lib,
  self,
  ...
}:

let
  # Reuse the Go daemon from the noctalia-plugins flake input — pure-Go
  # sqlite, CGO_ENABLED=0, so it builds unchanged on Darwin. The submodule
  # checkout under home/.config/… is for QML symlinking only and isn't
  # visible to flake eval without ?submodules=1.
  plugins = self.inputs.noctalia-plugins.packages.${pkgs.stdenv.hostPlatform.system};
  nostr-chatd = plugins.nostr-chatd;
  bar = self.packages.${pkgs.stdenv.hostPlatform.system}.nostr-chat-bar;

  # launchd has no XDG_RUNTIME_DIR; pin the socket so daemon and UI
  # agree without relying on confstr() returning identical paths from
  # Go and Swift (it does, but explicit beats clever).
  socket = "${config.home.homeDirectory}/.local/state/nostr-chatd/nostr-chatd.sock";
  logDir = "${config.home.homeDirectory}/.local/state/nostr-chatd";

  env = {
    NOSTR_CHAT_PEER_PUBKEY = "96dc8a8cb0c28bdd113c1f6e350abd6014c69369cbd618c3b8cd4d1326bf7e37";
    NOSTR_CHAT_RELAYS = lib.concatStringsSep "," [
      "wss://nostr.thalheim.io"
      "wss://nostr2.thalheim.io"
      "wss://nos.lol"
    ];
    NOSTR_CHAT_BLOSSOM = "https://nostr-files.thalheim.io";
    NOSTR_CHAT_DISPLAY_NAME = "Janet";
    NOSTR_CHAT_SECRET_CMD = "rbw get 'nostr identity'";
    PATH = lib.makeBinPath [
      pkgs.rbw
      pkgs.coreutils
    ];
  };
in
lib.mkIf pkgs.stdenv.isDarwin {
  launchd.enable = true;

  launchd.agents.nostr-chatd = {
    enable = true;
    config = {
      ProgramArguments = [
        "${nostr-chatd}/bin/nostr-chatd"
        "-socket"
        socket
      ];
      EnvironmentVariables = env;
      KeepAlive = true;
      RunAtLoad = true;
      ProcessType = "Background";
      StandardOutPath = "${logDir}/daemon.log";
      StandardErrorPath = "${logDir}/daemon.log";
    };
  };

  launchd.agents.nostr-chat-bar = {
    enable = true;
    config = {
      ProgramArguments = [
        "${bar}/bin/nostr-chat-bar"
        "--socket"
        socket
      ];
      KeepAlive = true;
      RunAtLoad = true;
      ProcessType = "Interactive";
      StandardOutPath = "${logDir}/bar.log";
      StandardErrorPath = "${logDir}/bar.log";
    };
  };
}
