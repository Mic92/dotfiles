{ config, lib, pkgs, ... }:

{
  services.telegraf.extraConfig.inputs = {
    net_response = map
      (host: {
        protocol = "tcp";
        address = "${host}.r:22";
        send = "SSH-2.0-Telegraf";
        expect = "SSH-2.0";
        tags.host = host;
        tags.org = "krebs";
        timeout = "10s";
      }) [
      "puyak"
      "yellow"
    ] ++ [{
      protocol = "tcp";
      address = "lassul.us:1935"; # rtmp
      tags.org = "krebs";
    } ];

    http_response = [
      {
        urls = [ "http://puyak.r" ];
        headers.Host = "light.shack";
        response_string_match = "shackspace";
        tags.host = "puyak";
        tags.org = "krebs";
      }
      {
        urls = [ "http://yellow.r:9091/transmission/web/" ];
        response_string_match = "Transmission Web";
        tags.host = "yellow";
        tags.org = "krebs";
      }
      {
        urls = [
          "http://yellow.r"
          "http://catalonia.r/"
        ];
        response_string_match = "Index of /";
        tags.org = "krebs";
      }
      {
        urls = [
          "http://wiki.r/Home"
        ];
        tags.host = "hotdog";
        response_string_match = "gollum";
        tags.org = "krebs";
      }
      {
        urls = [
          "http://search.r/"
        ];
        tags.host = "prism";
        response_string_match = "searx";
        tags.org = "krebs";
      }
      {
        urls = [
          "http://rss.r/?action=display&bridge=Heise&category=https%3A%2F%2Fwww.heise.de%2Fnewsticker%2Fheise-atom.xml&limit=5&format=Plaintext"
        ];
        tags.host = "news";
        response_string_match = "rss";
        tags.org = "krebs";
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
      }
      {
        urls = [
          "http://graph.r"
        ];
        tags.host = "gum";
        response_string_match = "tinc network map";
        tags.org = "krebs";
      }
      {
        urls = [
          "http://cgit.ni.r"
          "http://cgit.gum.r"
          "http://cgit.prism.r"
        ];
        response_string_match = "cgit";
        tags.org = "krebs";
      }
      {
        urls = [
          "http://build.hotdog.r"
        ];
        response_string_match = "Buildbot";
        tags.host = "hotdog";
        tags.org = "krebs";
      }
      {
        urls = [
          "http://p.r/1pziljc"
        ];
        response_string_match = "ok";
        tags.org = "krebs";
      }
    ];
  };
}
