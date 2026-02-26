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
    files.pi-auth-json.secret = true;

    prompts.pi-auth-json = {
      description = "Pi agent auth.json contents (OAuth credentials for Anthropic)";
      type = "hidden";
    };

    runtimeInputs = with pkgs; [ nak ];

    script = ''
      sk=$(nak key generate)
      pk=$(nak key public "$sk")
      echo -n "$sk" > "$out/nostr-private-key"
      echo -n "$pk" > "$out/nostr-public-key"
      cp "$prompts/pi-auth-json" "$out/pi-auth-json"
    '';
  };

  # Seed pi's auth.json before container start
  systemd.services."container@opencrow".preStart = let
    nostrKeyPath = config.clan.core.vars.generators.opencrow.files.nostr-private-key.path;
    authJsonPath = config.clan.core.vars.generators.opencrow.files.pi-auth-json.path;
  in ''
    install -d -m 0750 /var/lib/opencrow/pi-agent
    install -m 0600 ${authJsonPath} /var/lib/opencrow/pi-agent/auth.json
    install -m 0600 ${nostrKeyPath} /var/lib/opencrow/nostr-private-key
  '';

  services.opencrow = {
    enable = true;
    environment = {
      OPENCROW_BACKEND = "nostr";
      OPENCROW_NOSTR_PRIVATE_KEY_FILE = "/var/lib/opencrow/nostr-private-key";
      OPENCROW_NOSTR_RELAYS = "wss://nostr.0cx.de,wss://relay.damus.io,wss://relay.nostr.band,wss://nos.lol";
      OPENCROW_NOSTR_BLOSSOM_SERVERS = "https://blossom.nostr.build";
      OPENCROW_NOSTR_ALLOWED_USERS = "npub10yt4rh4g5t5kd47x7w8dpwqq7s228c53xacjqxvxjwu0kes3kzvsynqfu8";
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
