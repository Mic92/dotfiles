{ config, pkgs, ... }:
{
  imports = [ ./default.nix ];

  clan.core.vars.generators.calendar = {
    files.bot-password = {
      secret = true;
    };
    files.webhook-auth-token = {
      secret = true;
    };
    files.recovery-key = {
      secret = true;
    };

    prompts.password = {
      description = "Matrix bot password for clan-calendar-bot";
      type = "hidden";
      persist = true;
      display = {
        group = "clan.lol";
        label = "Calendar Bot Password";
      };
    };

    prompts.auth-token = {
      description = "n8n webhook authentication token";
      type = "hidden";
      display = {
        group = "clan.lol";
        label = "Webhook Auth Token";
      };
    };

    prompts.recovery-key = {
      description = "Element recovery key for device verification (optional)";
      type = "hidden";
      display = {
        group = "clan.lol";
        label = "Recovery Key (Optional)";
      };
    };

    runtimeInputs = with pkgs; [
      coreutils
      openssl
    ];

    script = ''
      # Copy Matrix bot password
      cat "$prompts/password" | tr -d '\n' > "$out/bot-password"

      # Generate or use provided auth token
      if [ -s "$prompts/auth-token" ]; then
        cat "$prompts/auth-token" | tr -d '\n' > "$out/webhook-auth-token"
      else
        # Generate random token if not provided
        openssl rand -base64 32 | tr -d '\n' > "$out/webhook-auth-token"
      fi

      # Copy recovery key if provided (optional)
      if [ -s "$prompts/recovery-key" ]; then
        cat "$prompts/recovery-key" | tr -d '\n' > "$out/recovery-key"
      else
        # Create empty file if not provided
        touch "$out/recovery-key"
      fi
    '';
  };

  # Enable PostgreSQL with clan-core module
  clan.core.postgresql = {
    enable = true;
    databases.calendar_bot = {
      restore.stopOnRestore = [ "calendar-bot.service" ];
      create.options = {
        TEMPLATE = "template0";
        ENCODING = "UTF8";
        LC_COLLATE = "C";
        LC_CTYPE = "C";
      };
    };
    users.calendar-bot = { };
  };

  services.calendar-bot = {
    enable = true;
    homeserver = "https://matrix.clan.lol";
    username = "@calendar:clan.lol";
    webhookUrl = "https://n8n.thalheim.io/webhook/calendar-bot";
    passwordFile = config.clan.core.vars.generators.calendar.files.bot-password.path;
    authTokenFile = config.clan.core.vars.generators.calendar.files.webhook-auth-token.path;
    recoveryKeyFile = config.clan.core.vars.generators.calendar.files.recovery-key.path;
  };
}
