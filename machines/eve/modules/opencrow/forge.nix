{
  config,
  lib,
  self,
  pkgs,
  ...
}:
let
  micsSkills = self.inputs.mics-skills;
  micsSkillsPkgs = micsSkills.packages.${pkgs.stdenv.hostPlatform.system};
in
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
    piPackage = self.inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.pi;
    extensions = {
      memory = true;
    };
    credentialFiles."nostr-private-key" =
      config.clan.core.vars.generators.opencrow-forge.files.nostr-private-key.path;
    skills =
      lib.genAttrs [
        "context7-cli"
        "pexpect-cli"
      ] (name: "${micsSkills}/skills/${name}")
      // {
        http = ./skills/http;
        gitea = ./skills/gitea;
      };
    environment = {
      TZ = "Europe/Berlin";
      OPENCROW_SOUL_FILE = "${./forge-soul.md}";
      OPENCROW_LOG_LEVEL = "info";
      OPENCROW_PI_PROVIDER = "anthropic";
      OPENCROW_PI_MODEL = "claude-opus-4-6";
      OPENCROW_BACKEND = "nostr";
      OPENCROW_NOSTR_PRIVATE_KEY_FILE = "%d/nostr-private-key";
      OPENCROW_NOSTR_RELAYS = "wss://nostr.0cx.de,wss://nos.lol,wss://nostr.thalheim.io";
      OPENCROW_NOSTR_DM_RELAYS = "wss://nos.lol,wss://nostr.thalheim.io,wss://nostr.0cx.de";
      OPENCROW_NOSTR_BLOSSOM_SERVERS = "https://nostr-files.thalheim.io";
      OPENCROW_NOSTR_ALLOWED_USERS = "npub10yt4rh4g5t5kd47x7w8dpwqq7s228c53xacjqxvxjwu0kes3kzvsynqfu8";
      OPENCROW_NOSTR_NAME = "forge";
      OPENCROW_NOSTR_DISPLAY_NAME = "Forge";
      OPENCROW_NOSTR_ABOUT = "Your coding coworker. Code, PRs, and technical tasks.";
    };
    extraPackages = [
      micsSkillsPkgs.context7-cli
      micsSkillsPkgs.pexpect-cli
    ]
    ++ (with pkgs; [
      cacert
      coreutils
      curl
      diffutils
      fd
      findutils
      git
      gnugrep
      gnused
      jq
      patch
      python3
      ripgrep
      tree
      which
    ]);
  };
}
