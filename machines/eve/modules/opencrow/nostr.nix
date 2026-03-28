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

  # Relay/backend config comes from instanceDefaults in default.nix.
  # Only Janet's identity lives here.
  services.opencrow.environment = {
    OPENCROW_NOSTR_NAME = "janet";
    OPENCROW_NOSTR_DISPLAY_NAME = "Janet";
    OPENCROW_NOSTR_ABOUT = "Not a robot. Not a girl. I'm Janet! 👋 An anthropomorphized vessel of knowledge, here to help.";
    OPENCROW_NOSTR_PICTURE = "https://robohash.org/96dc8a8cb0c28bdd113c1f6e350abd6014c69369cbd618c3b8cd4d1326bf7e37?set=set4&size=256x256";
  };
}
