let
  hosts = [
    "aarch64.nixos.community"
    "build01.nix-community.org"
    "build02.nix-community.org"
    "build03.nix-community.org"
    "build04.nix-community.org"
    "darwin01.nix-community.org"
    "darwin02.nix-community.org"
    "web01.nix-community.org"
  ];
in
{
  services.telegraf.extraConfig.inputs = {
    http_response = [
      {
        urls = [ "https://nur-update.nix-community.org/" ];
        response_string_match = "NUR update endpoint";
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
