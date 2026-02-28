{
  config,
  self,
  pkgs,
  ...
}:
{
  imports = [
    self.inputs.opencrow.nixosModules.default
  ];

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

  services.opencrow = {
    enable = true;
    piPackage = self.inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.pi;
    credentialFiles = {
      "nostr-private-key" = config.clan.core.vars.generators.opencrow.files.nostr-private-key.path;
    };
    environment = {
      OPENCROW_BACKEND = "nostr";
      OPENCROW_NOSTR_PRIVATE_KEY_FILE = "%d/nostr-private-key";
      OPENCROW_NOSTR_RELAYS = "wss://nostr.0cx.de,wss://relay.damus.io,wss://nos.lol";
      OPENCROW_NOSTR_DM_RELAYS = "wss://relay.damus.io,wss://nos.lol";
      OPENCROW_NOSTR_BLOSSOM_SERVERS = "https://blossom.nostr.build";
      OPENCROW_NOSTR_ALLOWED_USERS = "npub10yt4rh4g5t5kd47x7w8dpwqq7s228c53xacjqxvxjwu0kes3kzvsynqfu8";
      OPENCROW_LOG_LEVEL = "debug";
      OPENCROW_PI_PROVIDER = "anthropic";
      OPENCROW_PI_MODEL = "claude-sonnet-4-5";
    };
    extraPackages = with pkgs; [
      curl
      lynx
      jq
      w3m
    ];
  };
}
