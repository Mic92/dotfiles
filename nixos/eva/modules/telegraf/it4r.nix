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
        "search.warhelp.broenradio.org"
        "backend.warhelp.broenradio.org"
        # TODO
        "search.warhelp.eu"
        "backend.search.warhelp.eu"
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
      "2a01:4f8:10b:49f:1::1"
    ];
    http_response = [
      {
        urls = [
          "search.warhelp.broenradio.org"
          "search.warhelp.eu"
        ];
        response_string_match = "Beherbergung";
        tags.host = "eve";
        tags.org = "it4r";
      }
      {
        urls = [
          "backend.warhelp.broenradio.org"
          "backend.search.warhelp.eu"
        ];
        response_string_match = "Graphql";
        tags.host = "eve";
        tags.org = "it4r";
      }
    ];
  };
}
