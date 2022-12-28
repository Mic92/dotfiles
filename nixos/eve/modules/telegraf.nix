{
  services.telegraf.extraConfig.inputs = {
    ping =
      let
        mobileUrls = [
          "turingmachine.r"
          "herbert.r"
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
          urls = [
            "4.eva.thalheim.io"
          ];
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
  };

  systemd.services.telegraf.path = [ "/run/wrappers" ];
}
