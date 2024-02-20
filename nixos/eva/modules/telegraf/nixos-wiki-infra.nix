let
  hosts = [
    "wiki.staging.julienmalka.me"
  ];
in
{
  services.telegraf.extraConfig.inputs = {
    http_response = [
      {
        urls = [ "https://wiki.staging.julienmalka.me/wiki/Main_Page" ];
        response_string_match = "NixOS Wiki";
        tags.host = "nixos-wiki";
        tags.org = "nixos-wiki";
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
          tags.org = "nixos-wiki";
          timeout = "10s";
        })
        hosts;
  };
}
