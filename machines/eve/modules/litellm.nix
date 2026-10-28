# OpenAI-compatible gateway in front of our Qwen deployments. p0 is the
# primary backend; jack (self-hosted vLLM, often down for experiments) is
# only used when p0 fails. Clients see a single model name and one key.
{ config, pkgs, ... }:
let
  port = 4000;
  gen = config.clan.core.vars.generators.litellm;
  model = "qwen3.8-27b";
in
{
  clan.core.vars.generators.p0-inference = {
    files.api-key.secret = true;
    prompts.api-key.description = "API key for https://inference.p0.contact";
    script = ''
      cp "$prompts/api-key" "$out/api-key"
    '';
  };

  clan.core.vars.generators.litellm = {
    dependencies = [ "p0-inference" ];
    files.master-key.secret = true;
    files.env.secret = true;
    runtimeInputs = [ pkgs.openssl ];
    script = ''
      key="sk-$(openssl rand -hex 32)"
      printf '%s' "$key" > "$out/master-key"
      {
        echo "LITELLM_MASTER_KEY=$key"
        echo "P0_API_KEY=$(< "$in/p0-inference/api-key")"
      } > "$out/env"
    '';
  };

  services.litellm = {
    enable = true;
    host = "::";
    inherit port;
    environmentFile = gen.files.env.path;
    settings = {
      model_list = [
        {
          model_name = model;
          litellm_params = {
            model = "hosted_vllm/Qwen3.8-27B-FP8";
            api_base = "https://inference.p0.contact/v1";
            api_key = "os.environ/P0_API_KEY";
          };
        }
        {
          model_name = "${model}-jack";
          litellm_params = {
            model = "hosted_vllm/qwen3.8-27b";
            api_base = "http://jack.r:8000/v1";
            api_key = "dummy";
          };
        }
      ];
      router_settings = {
        fallbacks = [ { ${model} = [ "${model}-jack" ]; } ];
        num_retries = 1;
        # Skip a dead backend for a while instead of timing out on every request.
        allowed_fails = 1;
        cooldown_time = 60;
      };
      litellm_settings = {
        drop_params = true;
        request_timeout = 600;
      };
      general_settings.master_key = "os.environ/LITELLM_MASTER_KEY";
    };
  };

  # Only reachable from retiolum (and locally, e.g. the hermes container).
  networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [ port ];
}
