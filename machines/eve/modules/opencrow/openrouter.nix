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
  # Janet handles personal data, so routing is pinned per-request below:
  # data_collection=deny drops any provider that retains inputs, and the
  # order/allow_fallbacks lock traffic to Venice (privacy-first, no logging)
  # instead of silently spilling to a logging provider.
  services.opencrow.piModels.providers.openrouter = {
    baseUrl = "https://openrouter.ai/api/v1";
    api = "openai-completions";
    apiKey = "!rbw get openrouter-api-key";
    models = [
      {
        id = "google/gemma-4-26b-a4b-it";
        name = "Gemma 4 (26B A4B)";
        # Cap well below the provider's 256K so pi's auto-compaction fires
        # every turn instead of resending an ever-growing context (the
        # March logs showed ~380M prompt tokens from an uncompacted history).
        contextWindow = 65536;
        input = [
          "text"
          "image"
        ];
        compat.openRouterRouting = {
          data_collection = "deny";
          order = [ "Venice" ];
          allow_fallbacks = false;
        };
      }
    ];
  };
}
