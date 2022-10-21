{
  config,
  lib,
  pkgs,
  ...
}: {
  services.telegraf.extraConfig.inputs = {
    net_response =
      map
      (host: {
        protocol = "tcp";
        address = "6.${host}.r:22";
        send = "SSH-2.0-Telegraf";
        expect = "SSH-2.0";
        tags.host = host;
        tags.org = "krebs";
        timeout = "10s";
      }) [
        "hotdog"
        "yellow"
        "prism"
        #"puyak"
        "ni"
      ]
      ++ [
        {
          protocol = "tcp";
          address = "lassul.us:1935"; # rtmp
          tags.org = "krebs";
        }
      ];

    x509_cert = [
      {
        sources = [
          "https://catalonia.r:443"
        ];
        tags.host = "catalonia";
        tags.org = "krebs";
      }
    ];

    http_response = [
      #{
      #  urls = ["http://puyak.r"];
      #  headers.Host = "light.shack";
      #  response_string_match = "shackspace";
      #  tags.host = "puyak";
      #  tags.org = "krebs";
      #  interface = "tinc.retiolum";
      #}
      {
        urls = ["http://yellow.r:9091/transmission/web/"];
        response_string_match = "Transmission Web";
        tags.host = "yellow";
        tags.org = "krebs";
        interface = "tinc.retiolum";
      }
      {
        urls = [
          "http://yellow.r"
          "http://catalonia.r/"
          "https://catalonia.r/"
        ];
        response_string_match = "Index of /";
        tags.org = "krebs";
        interface = "tinc.retiolum";
      }
      {
        urls = [
          "http://wiki.r/Home"
        ];
        tags.host = "hotdog";
        response_string_match = "gollum";
        tags.org = "krebs";
        interface = "tinc.retiolum";
      }
      {
        urls = [
          "http://search.r/"
        ];
        tags.host = "prism";
        response_string_match = "searx";
        tags.org = "krebs";
        interface = "tinc.retiolum";
      }
      {
        urls = [
          "http://rss.r/?action=display&bridge=Heise&category=https%3A%2F%2Fwww.heise.de%2Fnewsticker%2Fheise-atom.xml&limit=5&format=Plaintext"
        ];
        tags.host = "news";
        response_string_match = "rss";
        tags.org = "krebs";
        interface = "tinc.retiolum";
      }
      {
        urls = [
          "http://news.r/"
          "http://brockman.r/"
          "http://go.r/"
        ];
        tags.host = "news";
        method = "HEAD";
        tags.org = "krebs";
        interface = "tinc.retiolum";
      }
      {
        urls = [
          "http://graph.r"
        ];
        tags.host = "gum";
        response_string_match = "tinc network map";
        tags.org = "krebs";
        interface = "tinc.retiolum";
      }
      {
        urls = [
          "http://cgit.ni.r"
          "http://cgit.gum.r"
          "http://cgit.prism.r"
        ];
        response_string_match = "cgit";
        tags.org = "krebs";
        interface = "tinc.retiolum";
      }
      {
        urls = [
          "http://build.hotdog.r"
        ];
        response_string_match = "Buildbot";
        tags.host = "hotdog";
        tags.org = "krebs";
        interface = "tinc.retiolum";
      }
      {
        urls = [
          "http://stable-confusion.r"
        ];
        response_string_match = "Stable Diffusion";
        tags.host = "jack";
        tags.org = "krebs";
        interface = "tinc.retiolum";
      }
      {
        urls = [
          "http://c.r/ok"
        ];
        response_string_match = "ok";
        tags.org = "krebs";
        interface = "tinc.retiolum";
      }
    ];
  };
}
