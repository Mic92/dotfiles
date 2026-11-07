{
  services.telegraf.httpSd.targets.uni = {
    name = "uni";
    url = "https://tum-dse.github.io/doctor-cluster-config/telegraf.json";
  };

  services.telegraf.extraConfig.inputs.x509_cert = [
    {
      sources = [ "https://web.dse.in.tum.de:443" ];
      tags.host = "vmbhatotia19";
      tags.org = "uni";
    }
  ];

  # vLLM serving qwen on jack (LiteLLM fallback backend on eve)
  services.telegraf.extraConfig.inputs.http_response = [
    {
      urls = [ "http://jack.r:8000/health" ];
      tags.host = "jack";
      tags.org = "uni";
      response_status_code = 200;
    }
  ];
}
