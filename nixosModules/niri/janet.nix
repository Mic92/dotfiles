# Janet — the local OpenCrow instance fronted by the noctalia chat panel.
# Plugin QML is symlinked from the noctalia-plugins submodule into
# ~/.config/noctalia/plugins/ by homeshick. The daemon + systemd unit
# come from the same repo via flake input so nix builds don't need
# ?submodules=1.
{ pkgs, self, ... }:
{
  imports = [
    self.inputs.noctalia-plugins.nixosModules.nostr-chat
  ];

  services.nostr-chat = {
    peerPubkey = "96dc8a8cb0c28bdd113c1f6e350abd6014c69369cbd618c3b8cd4d1326bf7e37";
    relays = [
      "wss://nostr.thalheim.io"
      "wss://nostr2.thalheim.io"
      "wss://nos.lol"
      # "wss://nostr.0cx.de"  # 2026-04: down/flaky
    ];
    blossom = [ "https://nostr-files.thalheim.io" ];
    displayName = "Janet";
    secretCommand = "rbw get 'nostr identity'";
    extraPath = with pkgs; [
      noctalia-shell
      rbw
    ];
  };

  # rbw-agent must be unlocked before the daemon asks for the nsec,
  # else `rbw get` spawns a pinentry into nowhere.
  systemd.user.services.nostr-chatd.after = [ "rbw-agent.service" ];
}
