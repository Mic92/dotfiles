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

  clan.core.vars.generators.hermes-inference = {
    files.api-key.secret = true;
    prompts.api-key.description = "API key for the OpenAI-compatible inference endpoint";
    script = ''
      cp "$prompts/api-key" "$out/api-key"
    '';
  };

  users.users.hermes = {
    isSystemUser = true;
    group = "hermes";
    uid = 2001;
  };
  users.groups.hermes.gid = 2001;
  nix.settings.extra-allowed-users = [ "hermes" ];

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
      "--load-credential=inference-api-key:${config.clan.core.vars.generators.hermes-inference.files.api-key.path}"
    ];

    config = _: {
      imports = [ ../agent-container.nix ];

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

      environment.systemPackages = [ hermesPkg ];

      systemd.tmpfiles.rules =
        let
          hermesConfig = pkgs.writers.writeYAML "hermes-config.yaml" {
            model.model = "Qwen3.6-27B-FP8";
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

        # Base tools come from the container profile (agent-container.nix).
        path = [
          hermesPkg
          "/run/current-system/sw"
        ];

        environment = {
          TZ = "Europe/Berlin";
          NIX_REMOTE = "daemon";
          HOME = stateDir;
          HERMES_HOME = "${stateDir}/.hermes";
          HERMES_INFERENCE_PROVIDER = "vllm";
          CUSTOM_BASE_URL = "https://inference.p0.contact/v1";
          HERMES_MODEL = "Qwen3.6-27B-FP8";
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
            "inference-api-key"
          ];
          Restart = "on-failure";
          RestartSec = 30;
          ExecStart = pkgs.writeShellScript "hermes-gateway" ''
            set -euo pipefail
            SLACK_BOT_TOKEN=$(< "$CREDENTIALS_DIRECTORY/slack-bot-token")
            SLACK_APP_TOKEN=$(< "$CREDENTIALS_DIRECTORY/slack-app-token")
            # hermes expects OAuth/setup tokens in ANTHROPIC_TOKEN (not ANTHROPIC_AUTH_TOKEN)
            ANTHROPIC_TOKEN=$(< "$CREDENTIALS_DIRECTORY/anthropic-api-key")
            OPENAI_API_KEY=$(< "$CREDENTIALS_DIRECTORY/inference-api-key")
            export SLACK_BOT_TOKEN SLACK_APP_TOKEN ANTHROPIC_TOKEN OPENAI_API_KEY
            exec ${lib.getExe hermesPkg} gateway run
          '';
        };
      };
    };
  };
}
