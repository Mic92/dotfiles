{
  config,
  pkgs,
  ...
}:
{
  # Forge gets its own Nostr identity, separate from Janet's.
  clan.core.vars.generators.opencrow-forge = {
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

  services.opencrow.instances.forge = {
    enable = true;

    credentialFiles."nostr-private-key" =
      config.clan.core.vars.generators.opencrow-forge.files.nostr-private-key.path;

    environment = {
      OPENCROW_SOUL_FILE = "${./forge-soul.md}";
      OPENCROW_LOG_LEVEL = "info";
      OPENCROW_PI_MODEL = "claude-opus-4-6";
      OPENCROW_NOSTR_NAME = "forge";
      OPENCROW_NOSTR_DISPLAY_NAME = "Forge";
      OPENCROW_NOSTR_ABOUT = "Your coding coworker. Code, PRs, and technical tasks.";
    };
  };
}
