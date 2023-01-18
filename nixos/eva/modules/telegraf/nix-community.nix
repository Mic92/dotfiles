let
  hosts = [
    "build01.nix-community.org"
    "build02.nix-community.org"
    "build03.nix-community.org"
    "build04.nix-community.org"
  ];
in
{
  services.telegraf.extraConfig.inputs = {
    http_response = [
      {
        urls = [ "https://search.nix-community.org/" ];
        response_string_match = "Hound";
        tags.host = "build03.nix-community.org";
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
