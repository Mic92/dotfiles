{
  config,
  lib,
  pkgs,
  self,
  ...
}:
let
  hermesPkg = self.inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.hermes-agent;
  stateDir = "/var/lib/hermes";
  gen = config.clan.core.vars.generators.hermes;
in
{
  # Slack credentials for the Hermes gateway.
  clan.core.vars.generators.hermes = {
    files.slack-bot-token.secret = true;
    files.slack-app-token.secret = true;
    files.anthropic-api-key.secret = true;

    prompts.slack-bot-token.description = "Slack bot token (xoxb-…) for the Hermes app";
    prompts.slack-app-token.description = "Slack app-level token (xapp-…) with connections:write";
    prompts.anthropic-api-key.description = "Anthropic token";

    script = ''
      cp "$prompts/slack-bot-token" "$out/slack-bot-token"
      cp "$prompts/slack-app-token" "$out/slack-app-token"
      cp "$prompts/anthropic-api-key" "$out/anthropic-api-key"
    '';
  };

  # The hermes user only exists inside the container; create the bind-mount
  # source as root here, ownership is fixed up from the inside.
  systemd.tmpfiles.rules = [
    "d ${stateDir} 0750 - - -"
  ];

  containers.hermes = {
    autoStart = true;
    privateNetwork = false;

    bindMounts.${stateDir} = {
      hostPath = stateDir;
      isReadOnly = false;
    };

    extraFlags = [
      "--load-credential=slack-bot-token:${gen.files.slack-bot-token.path}"
      "--load-credential=slack-app-token:${gen.files.slack-app-token.path}"
      "--load-credential=anthropic-api-key:${gen.files.anthropic-api-key.path}"
    ];

    config = _: {
      system.stateVersion = "25.05";

      # Copy the host's retiolum entries so jack.r resolves inside.
      networking.extraHosts = config.networking.extraHosts;

      users.users.hermes = {
        isSystemUser = true;
        group = "hermes";
        uid = 2001;
        home = stateDir;
      };
      users.groups.hermes.gid = 2001;

      environment.etc."timezone".text = "Europe/Berlin\n";

      systemd.tmpfiles.rules =
        let
          # Same routing policy as janet: deny data-collecting providers,
          # Venice only. Hermes only reads $HERMES_HOME/config.yaml.
          hermesConfig = pkgs.writers.writeYAML "hermes-config.yaml" {
            # Cap output tokens: the self-hosted vLLM (jack.r, A40) serves
            # qwen3-30b-a3b-instruct with --max-model-len 98304. Hermes'
            # default of 65536 output tokens leaves only ~32k for input,
            # causing HTTP 400 + "max compression attempts reached".
            # Auxiliary tasks (title generation, compression) fall back to
            # "gpt-4o-mini" when model.model is unset in config.yaml (env
            # HERMES_MODEL is not consulted there), which 404s on vLLM.
            model = {
              model = "qwen3-30b-a3b-instruct";
              max_tokens = 16384;
            };
            provider_routing = {
              data_collection = "deny";
              only = [ "venice" ];
            };
          };
        in
        [
          "d ${stateDir} 0750 hermes hermes -"
          "d ${stateDir}/.hermes 0750 hermes hermes -"
          "L+ ${stateDir}/.hermes/config.yaml - - - - ${hermesConfig}"
        ];

      systemd.services.hermes = {
        description = "Hermes Agent Slack gateway";
        wantedBy = [ "multi-user.target" ];
        after = [ "network-online.target" ];
        wants = [ "network-online.target" ];

        path = [
          hermesPkg
        ]
        ++ (with pkgs; [
          bash
          coreutils
          curl
          fd
          file
          findutils
          git
          gnugrep
          gnused
          gnutar
          gzip
          jq
          openssh
          procps
          ripgrep
          unzip
          util-linux
          which
          xz
        ]);

        environment = {
          TZ = "Europe/Berlin";
          HOME = stateDir;
          HERMES_HOME = "${stateDir}/.hermes";
          # "vllm" is an alias for hermes' "custom" OpenAI-compatible provider;
          # points at the self-hosted Qwen3 on jack's A40 (retiolum).
          HERMES_INFERENCE_PROVIDER = "vllm";
          CUSTOM_BASE_URL = "http://jack.r:8000/v1";
          HERMES_MODEL = "qwen3-30b-a3b-instruct";
          # No auth on the endpoint, but the OpenAI client wants a key.
          OPENAI_API_KEY = "unused";
          SLACK_ALLOWED_USERS = "U02TAKGUGF4";
        };

        serviceConfig = {
          User = "hermes";
          Group = "hermes";
          WorkingDirectory = stateDir;
          StateDirectory = "hermes";
          ImportCredential = [
            "slack-bot-token"
            "slack-app-token"
            "anthropic-api-key"
          ];
          Restart = "on-failure";
          RestartSec = 30;
          ExecStart = pkgs.writeShellScript "hermes-gateway" ''
            set -euo pipefail
            SLACK_BOT_TOKEN=$(< "$CREDENTIALS_DIRECTORY/slack-bot-token")
            SLACK_APP_TOKEN=$(< "$CREDENTIALS_DIRECTORY/slack-app-token")
            # hermes expects OAuth/setup tokens in ANTHROPIC_TOKEN (not ANTHROPIC_AUTH_TOKEN)
            ANTHROPIC_TOKEN=$(< "$CREDENTIALS_DIRECTORY/anthropic-api-key")
            export SLACK_BOT_TOKEN SLACK_APP_TOKEN ANTHROPIC_TOKEN
            exec ${lib.getExe hermesPkg} gateway run
          '';
        };
      };
    };
  };
}
