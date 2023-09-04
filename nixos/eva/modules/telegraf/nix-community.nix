let
  hosts = [
    "web02.nix-community.org"
  ];
in
{
  services.telegraf.extraConfig.inputs = {
    http_response = [
      {
        urls = [ "https://monitoring.nix-community.org/prometheus/graph" ];
        response_string_match = "Prometheus Time Series Collection";
        tags.host = "web02.nix-community.org";
        tags.org = "nix-community";
      }
    ];
    net_response =
      map
        (host: {
          protocol = "tcp";
          address = "${host}:22";
          send = "SSH-2.0-Telegraf";
          expect = "SSH-2.0";
          tags.host = host;
          tags.org = "nix-community";
          timeout = "10s";
        })
        hosts;
  };
}
