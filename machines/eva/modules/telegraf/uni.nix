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
}
