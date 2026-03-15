{
  config,
  pkgs,
  ...
}:
{
  clan.core.vars.generators.opencrow = {
    files.nostr-private-key.secret = true;
    files.nostr-public-key.secret = false;

    runtimeInputs = with pkgs; [ nak ];

    script = ''
      sk=$(nak key generate)
      pk=$(nak key public "$sk")
      echo -n "$sk" > "$out/nostr-private-key"
      echo -n "$pk" > "$out/nostr-public-key"
    '';
  };

  services.opencrow.credentialFiles."nostr-private-key" =
    config.clan.core.vars.generators.opencrow.files.nostr-private-key.path;

  services.opencrow.environment = {
    OPENCROW_BACKEND = "nostr";
    OPENCROW_NOSTR_PRIVATE_KEY_FILE = "%d/nostr-private-key";
    OPENCROW_NOSTR_RELAYS = "wss://nostr.0cx.de,wss://nos.lol,wss://nostr.thalheim.io";
    OPENCROW_NOSTR_DM_RELAYS = "wss://nos.lol,wss://nostr.thalheim.io,wss://nostr.0cx.de";
    OPENCROW_NOSTR_BLOSSOM_SERVERS = "https://blossom.nostr.build";
    OPENCROW_NOSTR_ALLOWED_USERS = "npub10yt4rh4g5t5kd47x7w8dpwqq7s228c53xacjqxvxjwu0kes3kzvsynqfu8";
    OPENCROW_NOSTR_NAME = "janet";
    OPENCROW_NOSTR_DISPLAY_NAME = "Janet";
    OPENCROW_NOSTR_ABOUT = "Not a robot. Not a girl. I'm Janet! 👋 An anthropomorphized vessel of knowledge, here to help.";
    OPENCROW_NOSTR_PICTURE = "https://robohash.org/96dc8a8cb0c28bdd113c1f6e350abd6014c69369cbd618c3b8cd4d1326bf7e37?set=set4&size=256x256";
  };
}
