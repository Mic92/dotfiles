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
        })
        [
          "hotdog"
          #"puyak"
          "ni"
        ];

    x509_cert = [
      {
        sources = [ "https://catalonia.r:443" ];
        tags.host = "catalonia";
        tags.org = "krebs";
      }
    ];

    http_response = [
      {
        urls = [ "http://wiki.r/Home" ];
        tags.host = "hotdog";
        response_string_match = "gollum";
        tags.org = "krebs";
        response_status_code = 200;
      }
      {
        urls = [ "http://ca.r/health" ];
        response_string_match = ''{"status":"ok"}'';
        tags.host = "hotdog";
        tags.org = "krebs";
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
      #  response_status_code = 200;
      #}
      {
        urls = [ "http://graph.r" ];
        tags.host = "gum";
        response_string_match = "tinc network map";
        tags.org = "krebs";
        response_status_code = 200;
      }
      {
        urls = [ "http://cgit.ni.r" ];
        response_string_match = "cgit";
        tags.host = "ni";
        tags.org = "krebs";
        response_status_code = 200;
      }

      {
        urls = [ "http://build.hotdog.r" ];
        response_string_match = "Buildbot";
        tags.host = "hotdog";
        tags.org = "krebs";
        response_status_code = 200;
      }

      {
        urls = [ "http://agenda.r" ];
        response_string_match = "Agenda";
        tags.host = "hotdog";
        tags.org = "krebs";
        response_status_code = 200;
      }
    ];
  };
}
