{
  config,
  ...
}:
{
  clan.core.vars.generators.opencrow-openrouter = {
    files.api-key.secret = true;

    prompts.api-key.description = "OpenRouter (openrouter.ai) API key";

    script = ''
      cp "$prompts/api-key" "$out/api-key"
    '';
  };

  services.opencrow.rbwEntries."openrouter-api-key" = "openrouter-api-key";

  services.opencrow.credentialFiles."openrouter-api-key" =
    config.clan.core.vars.generators.opencrow-openrouter.files.api-key.path;

  # OpenRouter brokers to third-party hosts; each has its own data policy.
  # Janet handles personal data, so the routing below (set per-request by
  # pi is not possible) can't pin providers — instead the account itself
  # must have "data_collection: deny" / ZDR enabled in OpenRouter settings.
  services.opencrow.piModels.providers.openrouter = {
    baseUrl = "https://openrouter.ai/api/v1";
    api = "openai-completions";
    apiKey = "!rbw get openrouter-api-key";
    models = [
      {
        id = "mistralai/mistral-small-3.2-24b-instruct";
        name = "Mistral Small 3.2 (24B)";
        contextWindow = 128000;
        input = [
          "text"
          "image"
        ];
      }
    ];
  };
}
