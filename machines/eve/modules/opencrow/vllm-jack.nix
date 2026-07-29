{
  # Self-hosted vLLM (Qwen3-30B-A3B-Instruct-2507-FP8) on jack's A40,
  # reachable over retiolum. No API key required; pi still wants a
  # non-empty value, so a placeholder is used.
  services.opencrow.piModels.providers.jack = {
    baseUrl = "http://jack.r:8000/v1";
    api = "openai-completions";
    apiKey = "unused";
    models = [
      {
        id = "qwen3-30b-a3b-instruct";
        name = "Qwen3 30B A3B Instruct (jack)";
        # Matches the server's --max-model-len; the A40's KV cache fits one
        # full-length request.
        contextWindow = 98304;
        input = [ "text" ];
        # vLLM's chat-completions endpoint rejects role:"developer".
        compat.supportsDeveloperRole = false;
      }
    ];
  };
}
