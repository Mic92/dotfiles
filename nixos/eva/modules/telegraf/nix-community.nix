let
  hosts = [
    "web02.nix-community.org"
  ];
in
{
  services.telegraf.extraConfig.inputs = {
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
