{
  imports = [
    ../../modules/telegraf.nix
  ];

  services.telegraf.extraConfig.inputs = {
    postfix = {};
    ping = let
      urls = [
        "eva.r"
        "eva.thalheim.io"
      ];
      mobileUrls = [
        "turingmachine.r"
        "herbert.r"
      ];
    in [{
      method = "native";
      urls = map (url: "${url}") mobileUrls;
      tags.type = "mobile";
      count = 5;
    } {
      method = "native";
      urls = map (url: "4.${url}") urls;
    } {
      method = "native";
      urls = map (url: "6.${url}") urls;
      ipv6 = true;
    }];
  };
}
