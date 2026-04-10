{
  config,
  ...
}:
{
  clan.core.vars.generators.opencrow-morpheus = {
    files.api-key.secret = true;

    prompts.api-key.description = "Morpheus (morpheus.cit.tum.de) API key";

    script = ''
      cp "$prompts/api-key" "$out/api-key"
    '';
  };

  services.opencrow.rbwEntries."morpheus-api-key" = "morpheus-api-key";

  services.opencrow.credentialFiles."morpheus-api-key" =
    config.clan.core.vars.generators.opencrow-morpheus.files.api-key.path;

  # Mirror ~/.pi/agent/models.json so the container's pi knows about
  # the TUM-internal endpoint. apiKey goes through the mock rbw which
  # resolves it from the systemd credential above.
  services.opencrow.piModels.providers.morpheus = {
    baseUrl = "https://morpheus.cit.tum.de/api/v1";
    api = "openai-completions";
    apiKey = "!rbw get morpheus-api-key";
    models = [
      {
        id = "mistralai/Ministral-3-14B-Instruct-2512";
        name = "Ministral 3 (14B)";
        contextLength = 32768;
      }
      {
        id = "google/gemma-4-31B-it";
        name = "Gemma 4 (31B)";
        contextLength = 65536;
      }
      {
        id = "Qwen/Qwen3.5-35B-A3B-FP8";
        name = "Qwen 3.5 (35B)";
        contextLength = 65536;
      }
      {
        id = "qwen-35-35b-coding";
        name = "Qwen 3.5 (35B) Coding";
        contextLength = 65536;
      }
    ];
  };
}
