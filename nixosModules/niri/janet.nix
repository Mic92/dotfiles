# Janet — the local OpenCrow instance fronted by the noctalia chat panel.
# Plugin + daemon live co-located under home/.config/noctalia/plugins/;
# this file just wires your peer/relays/secret into that module.
{ pkgs, ... }:
{
  imports = [
    ../../home/.config/noctalia/plugins/nostr-chat/module.nix
  ];

  services.nostr-chat = {
    peerPubkey = "96dc8a8cb0c28bdd113c1f6e350abd6014c69369cbd618c3b8cd4d1326bf7e37";
    relays = [
      "wss://nostr.thalheim.io"
      "wss://nos.lol"
      "wss://nostr.0cx.de"
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
