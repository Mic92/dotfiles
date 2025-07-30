{
  services.telegraf.extraConfig = {
    agent.skip_processors_after_aggregators = false;
    inputs = {
      ping =
        let
          mobileUrls = [
            "turingmachine.r"
            "bernie.r"
          ];
        in
        [
          {
            method = "native";
            urls = map (url: "${url}") mobileUrls;
            tags.type = "mobile";
            count = 5;
          }
          {
            method = "native";
            urls = [ "4.eva.thalheim.io" ];
          }
          {
            method = "native";
            urls = [
              "6.eva.r"
              "6.eva.thalheim.io"
            ];
            ipv6 = true;
          }
        ];

      prometheus = [
        {
          # harmonia
          urls = [ "http://127.0.0.1:5000/metrics" ];
          metric_version = 2;
        }
      ];
    };
  };

  systemd.services.telegraf.path = [ "/run/wrappers" ];
}
