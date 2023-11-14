{
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
      ];

    x509_cert = [
      {
        sources = [
          "https://catalonia.r:443"
        ];
        tags.host = "catalonia";
        tags.org = "krebs";
      }
      {
        sources = [
          "https://jitsi.lassul.us"
          "https://pad.lassul.us"
          "https://mumble.lassul.us"
          "https://lassul.us"
          "https://social.krebsco.de"
          "https://ca.r"
        ];
        tags.host = "prism";
        tags.org = "krebs";
      }
    ];

    http_response = [
      {
        urls = [
          "http://radio.lassul.us/radio.ogg.m3u"
        ];
        tags.host = "prism";
        tags.org = "krebs";
        response_status_code = 200;
      }
      #{
      #  urls = ["http://puyak.r"];
      #  headers.Host = "light.shack";
      #  response_string_match = "shackspace";
      #  tags.host = "puyak";
      #  tags.org = "krebs";
      #  interface = "tinc.retiolum";
      #}
      {
        urls = [
          "http://transmission.r/transmission/web/"
        ];
        response_string_match = "Transmission Web";
        tags.host = "yellow";
        tags.org = "krebs";
        interface = "tinc.retiolum";
        response_status_code = 200;
        response_timeout = "60s";
      }
      {
        urls = [
          "http://yellow.r/chatty/index.html"
        ];
        response_string_match = "shoutbox";
        tags.host = "yellow";
        tags.org = "krebs";
        interface = "tinc.retiolum";
        response_status_code = 200;
      }
      {
        urls = [
          "http://yellow.r"
          # currently broken
          #"http://sicily.r/"
          #"https://sicily.r/"
          "http://flix.r/"
        ];
        response_string_match = "Index of /";
        tags.org = "krebs";
        interface = "tinc.retiolum";
        response_status_code = 200;
      }
      {
        urls = [
          "http://wiki.r/Home"
        ];
        tags.host = "hotdog";
        response_string_match = "gollum";
        tags.org = "krebs";
        interface = "tinc.retiolum";
        response_status_code = 200;
      }
      {
        urls = [
          "http://ca.r/health"
        ];
        response_string_match = ''{"status":"ok"}'';
        tags.host = "hotdog";
        tags.org = "krebs";
        interface = "tinc.retiolum";
        response_status_code = 200;
      }
      {
        urls = [
          "http://calendar.r/.web/"
        ];
        tags.host = "hotdog";
        tags.org = "krebs";
        interface = "tinc.retiolum";
        response_status_code = 200;
      }
      # currently broken
      #{
      #  urls = [
      #    "http://rss.r/?action=display&bridge=HeiseBridge&category=https%3A%2F%2Fwww.heise.de%2Fnewsticker%2Fheise-atom.xml&limit=5&format=Atom"
      #  ];
      #  tags.host = "news";
      #  response_string_match = "rss";
      #  tags.org = "krebs";
      #  interface = "tinc.retiolum";
      #  response_status_code = 200;
      #}
      #{
      #  urls = [
      #    "http://news.r/"
      #    "http://brockman.r/"
      #  ];
      #  tags.host = "news";
      #  method = "HEAD";
      #  tags.org = "krebs";
      #  interface = "tinc.retiolum";
      #  response_status_code = 200;
      #}
      #{
      #  urls = [
      #    "http://go.r"
      #  ];
      #  tags.host = "news";
      #  method = "POST";
      #  headers."Content-Type" = "multipart/form-data; boundary=------------------------04afef05eaafa8d5";
      #  body = ''
      #    --------------------------04afef05eaafa8d5
      #    Content-Disposition: form-data; name="uri"

      #    google.com
      #    --------------------------04afef05eaafa8d5--
      #  '';
      #  tags.org = "krebs";
      #  interface = "tinc.retiolum";
      #  response_status_code = 200;
      #}
      {
        urls = [
          "http://graph.r"
        ];
        tags.host = "gum";
        response_string_match = "tinc network map";
        tags.org = "krebs";
        interface = "tinc.retiolum";
        response_status_code = 200;
      }
      {
        urls = [
          "http://cgit.ni.r"
          "http://cgit.gum.r"
          "http://cgit.orange.r"
        ];
        response_string_match = "cgit";
        tags.org = "krebs";
        interface = "tinc.retiolum";
        response_status_code = 200;
      }
      {
        urls = [
          "http://jelly.r/web/index.html"
        ];
        response_string_match = "Jellyfin";
        tags.host = "prism";
        tags.org = "krebs";
        interface = "tinc.retiolum";
        response_status_code = 200;
      }
      {
        urls = [
          "https://jitsi.lassul.us"
        ];
        response_string_match = "Jitsi Meet";
        tags.host = "prism";
        tags.org = "krebs";
        response_status_code = 200;
      }
      {
        urls = [
          "https://pad.lassul.us"
        ];
        response_string_match = "HedgeDoc";
        tags.host = "prism";
        tags.org = "krebs";
        response_status_code = 200;
      }
      {
        urls = [
          "https://social.krebsco.de"
        ];
        response_string_match = "Mastodon";
        tags.host = "prism";
        tags.org = "krebs";
        response_status_code = 200;
      }
      {
        urls = [
          "http://build.hotdog.r"
        ];
        response_string_match = "Buildbot";
        tags.host = "hotdog";
        tags.org = "krebs";
        interface = "tinc.retiolum";
        response_status_code = 200;
      }
      {
        urls = [
          "http://stable-confusion.r"
        ];
        response_string_match = "Stable Diffusion";
        tags.host = "jack";
        tags.org = "krebs";
        interface = "tinc.retiolum";
        response_status_code = 200;
      }
      {
        urls = [
          "http://c.r/ok"
        ];
        response_string_match = "ok";
        tags.org = "krebs";
        interface = "tinc.retiolum";
        response_status_code = 200;
      }
      {
        urls = [
          "http://p.r"
        ];
        body = "ok";
        method = "POST";
        tags.org = "krebs";
        interface = "tinc.retiolum";
        response_status_code = 200;
      }
      {
        urls = [
          "http://agenda.r"
        ];
        response_string_match = "Agenda";
        tags.org = "krebs";
        interface = "tinc.retiolum";
        response_status_code = 200;
      }
      {
        urls = [
          "http://calendar.r/.web/"
        ];
        response_string_match = "Login";
        tags.org = "krebs";
        interface = "tinc.retiolum";
        response_status_code = 200;
      }
    ];
  };
}
