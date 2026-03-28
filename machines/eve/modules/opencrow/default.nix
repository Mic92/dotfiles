{
  lib,
  self,
  pkgs,
  ...
}:
let
  micsSkills = self.inputs.mics-skills;
  micsSkillsPkgs = micsSkills.packages.${pkgs.stdenv.hostPlatform.system};

  # Defaults injected into every opencrow instance via submodule merging.
  # Keeps infrastructure (relays, base tooling) in one place while each
  # agent file only declares what makes it distinct.
  instanceDefaults = {
    piPackage = lib.mkDefault self.inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.pi;

    extensions = {
      memory = lib.mkDefault true;
      reminders = lib.mkDefault true;
    };

    skills = {
      context7-cli = lib.mkDefault "${micsSkills}/skills/context7-cli";
      pexpect-cli = lib.mkDefault "${micsSkills}/skills/pexpect-cli";
      http = lib.mkDefault ./skills/http;
    };

    environment = {
      TZ = lib.mkDefault "Europe/Berlin";
      OPENCROW_PI_PROVIDER = lib.mkDefault "anthropic";

      # Periodic awareness check. Agent reads HEARTBEAT.md in its session
      # dir for the checklist. The reminder dispatcher runs regardless.
      OPENCROW_HEARTBEAT_INTERVAL = lib.mkDefault "30m";

      # Nostr infrastructure — shared across all bots, identity set per-agent.
      OPENCROW_BACKEND = lib.mkDefault "nostr";
      OPENCROW_NOSTR_PRIVATE_KEY_FILE = lib.mkDefault "%d/nostr-private-key";
      OPENCROW_NOSTR_RELAYS = lib.mkDefault "wss://nostr.0cx.de,wss://nos.lol,wss://nostr.thalheim.io";
      OPENCROW_NOSTR_DM_RELAYS = lib.mkDefault "wss://nos.lol,wss://nostr.thalheim.io,wss://nostr.0cx.de";
      OPENCROW_NOSTR_BLOSSOM_SERVERS = lib.mkDefault "https://nostr-files.thalheim.io";
      OPENCROW_NOSTR_ALLOWED_USERS = lib.mkDefault "npub10yt4rh4g5t5kd47x7w8dpwqq7s228c53xacjqxvxjwu0kes3kzvsynqfu8";
    };

    # Baseline tool set every bot container needs for pi to do useful work.
    extraPackages = [
      micsSkillsPkgs.context7-cli
      micsSkillsPkgs.pexpect-cli
    ]
    ++ (with pkgs; [
      bc
      cacert
      coreutils
      curl
      diffutils
      fd
      file
      findutils
      git
      gnugrep
      gnused
      gnutar
      gzip
      htmlq
      hurl
      jq
      less
      libarchive
      openssh
      patch
      procps
      python3
      ripgrep
      tree
      unzip
      util-linux
      w3m
      wget
      which
      xz
      yq-go
      zip
      zstd
    ]);
  };
in
{
  imports = [
    self.inputs.opencrow.nixosModules.default
    ./janet.nix
    ./forge.nix
  ];

  # Extend the upstream submodule type so every named instance picks up
  # the defaults above through normal module merging.
  options.services.opencrow = lib.mkOption {
    type = lib.types.submodule {
      options.instances = lib.mkOption {
        type = lib.types.attrsOf (lib.types.submodule { config = instanceDefaults; });
      };
      # The top-level (Janet's "default" instance) is a sibling submodule,
      # so apply the same defaults there too.
      config = instanceDefaults;
    };
  };
}
