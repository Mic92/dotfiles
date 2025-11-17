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
    '';
  };

  services.calendar-bot = {
    enable = true;
    homeserver = "https://matrix.clan.lol";
    username = "@calendar:clan.lol";
    webhookUrl = "https://n8n.thalheim.io/webhook/calendar-bot";
    passwordFile = config.clan.core.vars.generators.calendar.files.bot-password.path;
    authTokenFile = config.clan.core.vars.generators.calendar.files.webhook-auth-token.path;
  };
}
