# OpenAI-compatible vLLM endpoint at inference.p0.contact. The API key
# is a secret (clan var / systemd credential); the URL stays plain
# because pi does not resolve "!command" for baseUrl.
{ config, ... }:
{
  clan.core.vars.generators.opencrow-p0 = {
    files.api-key.secret = true;

    prompts.api-key.description = "p0 inference API key";

    script = ''
      cp "$prompts/api-key" "$out/api-key"
    '';
  };

  services.opencrow.rbwEntries."p0-api-key" = "p0-api-key";

  services.opencrow.credentialFiles."p0-api-key" =
    config.clan.core.vars.generators.opencrow-p0.files.api-key.path;

  services.opencrow.piModels.providers.p0 = {
    baseUrl = "https://inference.p0.contact/v1";
    api = "openai-completions";
    apiKey = "!rbw get p0-api-key";
    models = [
      {
        id = "Qwen3.6-27B-FP8";
        name = "Qwen 3.6 (27B, p0)";
        # Matches the server's max_model_len (262144).
        contextWindow = 262144;
        input = [ "text" ];
        # vLLM's chat-completions endpoint rejects role:"developer".
        compat.supportsDeveloperRole = false;
      }
    ];
  };
}
