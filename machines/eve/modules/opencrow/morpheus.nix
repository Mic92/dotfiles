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
    # contextWindow values are the vLLM server's max_model_len, not the
    # architectural limits — morpheus caps Gemma/Qwen at 65K and
    # Ministral at 32K regardless of what the model cards claim.
    #
    # All four backends are multimodal (verified against /v1/models
    # capabilities + an actual image round-trip); without input=[…,image]
    # pi's openai-completions adapter silently strips image parts before
    # the request ever leaves the box.
    models = [
      {
        id = "mistralai/Ministral-3-14B-Instruct-2512";
        name = "Ministral 3 (14B)";
        contextWindow = 32768;
        input = [
          "text"
          "image"
        ];
      }
      {
        id = "google/gemma-4-31B-it";
        name = "Gemma 4 (31B)";
        contextWindow = 65536;
        input = [
          "text"
          "image"
        ];
      }
      {
        id = "Qwen/Qwen3.5-35B-A3B-FP8";
        name = "Qwen 3.5 (35B)";
        contextWindow = 65536;
        reasoning = true;
        input = [
          "text"
          "image"
        ];
      }
      {
        id = "qwen-35-35b-coding";
        name = "Qwen 3.5 (35B) Coding";
        contextWindow = 65536;
        reasoning = true;
        input = [
          "text"
          "image"
        ];
      }
    ];
  };
}
