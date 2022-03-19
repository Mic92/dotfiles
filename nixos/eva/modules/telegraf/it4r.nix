{
  config,
  lib,
  pkgs,
  ...
}: let
in {
  services.telegraf.extraConfig.inputs = {
    x509_cert = [{
      sources = [
        "https://search.warhelp.broenradio.org:443"
        "https://backend.warhelp.broenradio.org:443"
        "https://search.warhelp.eu:443"
        "https://backend.search.warhelp.eu:443"
        # mission lifeline
        "https://search.beherbergung.broenradio.org:443"
        "https://backend.beherbergung.broenradio.org:443"
      ];
      tags.host = "eve";
      tags.org = "it4r";
    }];
    net_response = map
    (host: {
      protocol = "tcp";
      address = "${host}:22";
      send = "SSH-2.0-Telegraf";
      expect = "SSH-2.0";
      tags.host = host;
      tags.org = "it4r";
      timeout = "10s";
    }) [
      "[2a01:4f8:10b:49f:1::1]"
    ];
    http_response = [
      {
        urls = [
          "https://search.warhelp.broenradio.org"
          "https://search.warhelp.eu"
        ];
        response_string_match = "Beherbergung";
        tags.host = "eve";
        tags.org = "it4r";
      }
      {
        urls = [
          "https://backend.warhelp.broenradio.org"
          "https://backend.search.warhelp.eu"
        ];
        response_string_match = "Graphql";
        tags.host = "eve";
        tags.org = "it4r";
      }
      {
        urls = [
          "https://backend.warhelp.broenradio.org/health"
          "https://backend.search.warhelp.eu/health"
          "https://search.warhelp.broenradio.org/health"
          "https://search.warhelp.eu/health"
        ];
        response_string_match = "ok";
        tags.host = "eve";
        tags.org = "it4r";
      }
    ];
  };
}
