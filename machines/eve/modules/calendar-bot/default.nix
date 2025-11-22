{
  self,
  pkgs,
  config,
  lib,
  ...
}:
{
  options.services.calendar-bot = {
    enable = lib.mkEnableOption "Matrix calendar bot for n8n integration";

    homeserver = lib.mkOption {
      type = lib.types.str;
      default = "https://matrix.thalheim.io";
      description = "Matrix homeserver URL";
    };

    username = lib.mkOption {
      type = lib.types.str;
      description = "Matrix bot username";
      example = "@calendar-bot:thalheim.io";
    };

    webhookUrl = lib.mkOption {
      type = lib.types.str;
      description = "n8n webhook URL for bot commands";
      example = "https://n8n.thalheim.io/webhook/calendar-bot";
    };

    passwordFile = lib.mkOption {
      type = lib.types.path;
      description = "Path to file containing Matrix bot password";
    };

    authTokenFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = "Path to file containing n8n webhook authentication token";
    };

    recoveryKeyFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = "Path to file containing Element recovery key for device verification";
    };
  };

  config = lib.mkIf config.services.calendar-bot.enable {
    systemd.services.calendar-bot = {
      description = "Matrix Calendar Bot";
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];

      serviceConfig = {
        Type = "simple";
        User = "calendar-bot";
        Group = "calendar-bot";
        DynamicUser = true;
        StateDirectory = "calendar-bot";
        WorkingDirectory = "/var/lib/calendar-bot";

        LoadCredential = [
          "password:${config.services.calendar-bot.passwordFile}"
        ]
        ++ lib.optional (
          config.services.calendar-bot.authTokenFile != null
        ) "auth-token:${config.services.calendar-bot.authTokenFile}"
        ++ lib.optional (
          config.services.calendar-bot.recoveryKeyFile != null
        ) "recovery-key:${config.services.calendar-bot.recoveryKeyFile}";

        ExecStart =
          let
            args = [
              "${self.packages.${pkgs.system}.calendar-bot}/bin/calendar-bot"
              "--homeserver"
              config.services.calendar-bot.homeserver
              "--username"
              config.services.calendar-bot.username
              "--password-file"
              "\${CREDENTIALS_DIRECTORY}/password"
              "--webhook-url"
              config.services.calendar-bot.webhookUrl
              "--store-path"
              "/var/lib/calendar-bot/store"
            ]
            ++ lib.optionals (config.services.calendar-bot.authTokenFile != null) [
              "--auth-token-file"
              "\${CREDENTIALS_DIRECTORY}/auth-token"
            ]
            ++ lib.optionals (config.services.calendar-bot.recoveryKeyFile != null) [
              "--recovery-key-file"
              "\${CREDENTIALS_DIRECTORY}/recovery-key"
            ];
          in
          lib.escapeShellArgs args;

        # Security hardening
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        NoNewPrivileges = true;
        PrivateDevices = true;
        ProtectKernelTunables = true;
        ProtectControlGroups = true;
        RestrictSUIDSGID = true;
        LockPersonality = true;
        RestrictRealtime = true;
        RestrictNamespaces = true;
        SystemCallArchitectures = "native";

        Restart = "always";
        RestartSec = "30s";
      };
    };
  };
}
