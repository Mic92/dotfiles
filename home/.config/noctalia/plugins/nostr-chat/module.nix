# NixOS module for the nostr-chat noctalia plugin.
#
# Self-contained: builds the co-located ./daemon and runs it as a
# systemd *user* unit — the daemon needs the logged-in user's secret
# store, XDG_RUNTIME_DIR socket, and noctalia-shell session, so a
# system service wouldn't work.
#
#   imports = [ .../nostr-chat/module.nix ];
#   services.nostr-chat = {
#     peerPubkey = "96dc8a8c…";
#     relays = [ "wss://nostr.thalheim.io" "wss://nos.lol" ];
#     blossom = [ "https://nostr-files.thalheim.io" ];
#     displayName = "Janet";
#   };

{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.nostr-chat;
  shelld = pkgs.callPackage ./daemon { };
in
{
  options.services.nostr-chat = {
    peerPubkey = lib.mkOption {
      type = lib.types.str;
      description = "Hex pubkey of the NIP-17 peer (bot) to DM.";
      example = "96dc8a8cb0c28bdd113c1f6e350abd6014c69369cbd618c3b8cd4d1326bf7e37";
    };
    relays = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      description = "Relays both you and the peer listen on.";
      example = [
        "wss://nostr.thalheim.io"
        "wss://nos.lol"
      ];
    };
    blossom = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Blossom servers for image upload. Empty disables attachments.";
    };
    secretCommand = lib.mkOption {
      type = lib.types.str;
      example = "rbw get 'nostr identity'";
      description = ''
        Shell command that prints your nsec (or hex secret key) to stdout.
        Runs via `sh -c` in the user session, so anything works:
        `rbw get …`, `pass show …`, `cat /run/agenix/nsec`,
        `gopass show -o …`.
      '';
    };
    displayName = lib.mkOption {
      type = lib.types.str;
      default = "Chat";
      description = "Name shown in the panel header.";
    };
    extraPath = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ pkgs.noctalia-shell ];
      description = ''
        Packages on the daemon's PATH. noctalia-shell is required for IPC
        push; add whatever your secretCommand needs (rbw, pass, …).
      '';
    };
  };

  config = {
    systemd.user.services.nostr-chatd = {
      description = "Nostr NIP-17 bridge for noctalia-shell";
      after = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      wantedBy = [ "graphical-session.target" ];
      path = cfg.extraPath;
      environment = {
        NOSTR_CHAT_PEER_PUBKEY = cfg.peerPubkey;
        NOSTR_CHAT_RELAYS = lib.concatStringsSep "," cfg.relays;
        NOSTR_CHAT_BLOSSOM = lib.concatStringsSep "," cfg.blossom;
        NOSTR_CHAT_SECRET_CMD = cfg.secretCommand;
        NOSTR_CHAT_DISPLAY_NAME = cfg.displayName;
      };
      serviceConfig = {
        ExecStart = "${shelld}/bin/nostr-chatd";
        Restart = "on-failure";
        RestartSec = "5s";
      };
    };
  };
}
