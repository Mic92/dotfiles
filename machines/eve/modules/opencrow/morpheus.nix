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
        id = "mistralai/Ministral-3-14B-Reasoning-2512";
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
        # NOTE 2026-05: morpheus re-attached `async_context_compression`
        # and `context_length_filter_tokens_turns` to BOTH this id and the
        # qwen-35-35b-coding alias. async_context_compression rewrites past
        # tool outputs to "... [Tool outputs trimmed]" which the model then
        # parrots back as its reply. No filter-free model is currently
        # available; admin asked to restore the unfiltered alias or enable
        # ENABLE_OPENAI_API_PASSTHROUGH.
        id = "Qwen/Qwen3.6-35B-A3B";
        name = "Qwen 3.6 (35B)";
        contextWindow = 65536;
        reasoning = true;
        input = [
          "text"
          "image"
        ];
        # vLLM's chat-completions endpoint rejects role:"developer" and
        # reasoning_effort:"minimal"; pi defaults both for unknown
        # reasoning-capable openai-completions providers.
        # thinkingFormat="qwen-chat-template" lets pi drive the jinja
        # `enable_thinking` flag — callers that don't pass a
        # reasoningEffort (memory/compaction side-calls) get instruct
        # mode and finish in ~20 tokens instead of looping in <think>.
        compat = {
          supportsDeveloperRole = false;
          thinkingFormat = "qwen-chat-template";
          reasoningEffortMap = {
            minimal = "low";
            xhigh = "high";
          };
        };
      }
      {
        id = "qwen-35-35b-coding";
        name = "Qwen 3.6 (35B, coding alias)";
        contextWindow = 65536;
        reasoning = true;
        input = [
          "text"
          "image"
        ];
        compat = {
          supportsDeveloperRole = false;
          thinkingFormat = "qwen-chat-template";
          reasoningEffortMap = {
            minimal = "low";
            xhigh = "high";
          };
        };
      }
    ];
  };
}
