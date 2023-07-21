let
  hosts = [ "clan.lol" ];
in
{
  services.telegraf.extraConfig.inputs = {
    http_response = [
      {
        urls = [ "https://clan.lol/" ];
        response_string_match = "cLAN";
        tags.host = "clan.lol";
        tags.org = "nix-community";
      }
      {
        urls = [ "https://git.clan.lol/" ];
        response_string_match = "cLAN";
        tags.host = "clan.lol";
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
