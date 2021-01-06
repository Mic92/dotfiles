{ config, lib, pkgs, ... }:

{
  sops.secrets.telegraf.owner = config.systemd.services.telegraf.serviceConfig.User;
  sops.secrets.telegraf-shared = {
    owner = config.systemd.services.telegraf.serviceConfig.User;
    sopsFile = ../../secrets/telegraf.yaml;
  };
  systemd.services.telegraf.serviceConfig.SupplementaryGroups = [ "keys" ];

  imports = [
    ../../modules/telegraf.nix
  ];

  services.nginx.virtualHosts."telegraf.thalheim.io" = {
    forceSSL = true;
    enableACME = true;
    locations."/".extraConfig =  ''
      proxy_pass http://localhost:8186/;
    '';
  };

  services.telegraf = {
    environmentFiles = [
      config.sops.secrets.telegraf.path
      config.sops.secrets.telegraf-shared.path
    ];
    extraConfig = {
      agent.interval = lib.mkForce "120s";
      inputs = {
        influxdb_v2_listener = [{
          service_address = ":8186";
          token = ''''${INFLUXDB_PASSWORD}'';
        }];
        ping = let
          urls = [
            "eve.r"
            "eve.thalheim.io"

            # university
            "rose.r"
            "martha.r"
            "donna.r"
            "amy.r"
            "clara.r"
            "doctor.r"
          ];
          mobileUrls = [
            "turingmachine.r"
            "herbert.r"
          ];
        in
          (map (url: {
            method = "native";
            urls = [ url ];
            tags.type = "mobile";
            tags.host = lib.removeSuffix ".r" url;
            count = 5;
          }) mobileUrls) ++
          (map (url: {
            method = "native";
            urls = [ "4.${url}" ];
            tags.host = lib.removeSuffix ".r" url;
          }) urls) ++
          (map (url: {
            method = "native";
            urls = [ "6.${url}" ];
            ipv6 = true;
            tags.host = lib.removeSuffix ".r" url;
          }) urls);
        net_response = map (port: {
          protocol = "tcp";
          tags.host = "eve";
          address = "devkid.net:${toString port}";
        }) [
          30033 # ts3_ft
          10011 # ts3_sq
        ] ++ map (port: {
          protocol = "udp";
          tags.host = "eve";
          address = "devkid.net:${toString port}";
        }) [
        # telegraf also wants payload for udp...
        #  9987  # ts3_devkid
        #  22222 # ts3_martijn
        #  5037  # ts3_martin
        #  9000  # ts3_putzy
        ] ++ [{
          # imap
          protocol = "tcp";
          tags.host = "eve";
          address = "imap.thalheim.io:143";
        } {
          # imaps
          protocol = "tcp";
          tags.host = "eve";
          address = "imap.thalheim.io:993";
        } {
          # sieve
          protocol = "tcp";
          tags.host = "eve";
          address = "imap.thalheim.io:4190";
        } {
          # xmpp-client
          protocol = "tcp";
          tags.host = "eve";
          address = "jabber.thalheim.io:5222";
        } {
          # xmpp-server
          protocol = "tcp";
          tags.host = "eve";
          address = "jabber.thalheim.io:5269";
        } {
          # openldap
          protocol = "tcp";
          tags.host = "eve";
          address = "eve.r:389";
        } {
          # openldap
          protocol = "tcp";
          tags.host = "eva";
          address = "eva.r:389";
        } {
          # postfix: smtp
          protocol = "tcp";
          # amazon does block port 25
          tags.host = "eve";
          address = "eve.r:25";
        } {
          # postfix: submission
          protocol = "tcp";
          tags.host = "eve";
          address = "mail.thalheim.io:587";
        } {
          # postfix: smtps
          protocol = "tcp";
          tags.host = "eve";
          address = "mail.thalheim.io:465";
        } {
          protocol = "tcp";
          address = "eve.thalheim.io:22";
          tags.host = "eve";
          send = "SSH-2.0-Telegraf";
          expect = "SSH-2.0";
        }] ++ map (host: {
          protocol = "tcp";
          address = "${host}.r:22";
          tags.host = host;
          send = "SSH-2.0-Telegraf";
          expect = "SSH-2.0";
        }) [
          "rock"
          "eve"
          "amy"
          "donna"
          "clara"
          "martha"
          "rose"
          "doctor"
        ] ++ map (host: {
          protocol = "tcp";
          address = "${host}.r:22";
          send = "SSH-2.0-Telegraf";
          expect = "SSH-2.0";
          tags.host = host;
          tags.org = "krebs";
        }) [
          "puyak"
          "yellow"
        ] ++ [{
          protocol = "tcp";
          address = "lassul.us:1935"; # rtmp
          tags.org = "krebs";
        }];

        http = [{
          urls = [
            "https://api.github.com/repos/Mic92/nur-packages/commits/master/check-suites"
            "https://api.github.com/repos/Mic92/sops-nix/commits/master/check-suites"
          ];
          data_format = "json";
          json_query = "check_suites.#(app.id == 15368)";
        }];

        http_response = [{
          urls = [ "http://puyak.r" ];
          headers.Host = "light.shack";
          response_string_match = "shackspace";
          tags.host = "puyak";
          tags.org = "krebs";
        } {
          urls = [ "http://yellow.r:9091/transmission/web/" ];
          response_string_match = "Transmission Web";
          tags.host = "yellow";
          tags.org = "krebs";
        } {
          urls = [
            "http://helsinki.r"
            "http://yellow.r"
          ];
          response_string_match = "Index of /";
          tags.org = "krebs";
        } {
          urls = [
            "http://wiki.r/Home"
          ];
          tags.host = "hotdog";
          response_string_match = "gollum";
          tags.org = "krebs";
        } {
          urls = [
            "http://graph.r"
          ];
          tags.host = "gum";
          response_string_match = "Retiolum";
          tags.org = "krebs";
        } {
          urls = [
            "http://cgit.ni.r"
            "http://cgit.enklave.r"
            "http://cgit.gum.r"
            "http://cgit.prism.r"
          ];
          response_string_match = "cgit";
          tags.org = "krebs";
        } {
          urls = [
            "http://build.hotdog.r"
          ];
          response_string_match = "BuildBot";
          tags.host = "hotdog";
          tags.org = "krebs";
        } {
          urls = [
            "http://paste.r/"
          ];
          response_string_match = "Bepasty";
          tags.host = "prism";
          tags.org = "krebs";
        } {
          urls = [
            "http://p.r/1pziljc"
          ];
          response_string_match = "ok";
          tags.org = "krebs";
        } {
          urls = [ "https://www.wikipedia.org/" ];
          http_proxy = ''https://telegraf%40thalheim.io:''${LDAP_PASSWORD}@devkid.net:8889'';
          tags.host = "eve";
          response_string_match = "wikipedia.org";
        } {
          urls = [ "https://adminer.thalheim.io/" ];
          tags.host = "eve";
          response_string_match = "Login";
        } {
          urls = [ "https://mail.thalheim.io" ];
          tags.host = "eve";
          response_string_match = "javascript";
        } {
          urls = [ "https://ist.devkid.net/wiki/Hauptseite" ];
          tags.host = "eve";
          response_string_match = "Informationssystemtechnik";
        } {
          urls = [ "https://rss.devkid.net" ];
          tags.host = "eve";
        } {
          urls = [ "https://rspamd.thalheim.io" ];
          tags.host = "eve";
          response_string_match = "Rspamd";
        } {
          urls = [ "https://glowing-bear.thalheim.io" ];
          tags.host = "eve";
          response_string_match = "Glowing";
        } {
          urls = [ "https://grafana.thalheim.io/login" ];
          tags.host = "eve";
          response_string_match = "Grafana";
        } {
          tags.host = "eve";
          urls = [ "https://dl.thalheim.io/OtNjoZOUnEn3H6LJZ1qcIw/test" ];
        } {
          urls = [ "https://dns.thalheim.io/dns-query?dns=q80BAAABAAAAAAAAA3d3dwdleGFtcGxlA2NvbQAAAQAB" ];
          tags.host = "eve";
          response_string_match = "example";
        } {
          urls = [ "https://syncthing.thalheim.io" ];
          username = "syncthing";
          password = "$SYNCTHING_PASSWORD";
          tags.host = "eve";
          response_string_match = "Syncthing";
        } {
          urls = [ "https://git.thalheim.io" ];
          tags.host = "eve";
          response_string_match = "Gitea";
        } {
          urls = [ "https://thalheim.io" ];
          tags.host = "eve";
          response_string_match = "Higgs-Boson";
        } {
          urls = [ "http://loki.r/ready" ];
          tags.host = "rock";
          response_string_match = "ready";
        } {
          urls = [
            "https://cloud.thalheim.io/login"
            "https://pim.devkid.net/login"
          ];
          tags.host = "eve";
          response_string_match = "Nextcloud";
        } {
          urls = ["https://influxdb.thalheim.io:8086/ping"];
          tags.host = "eve";
        }];

        dns_query = {
          servers = [
            "ns1.thalheim.io"
            "ns2.he.net"
            "ns3.he.net"
            "ns4.he.net"
            "ns5.he.net"
          ];
          domains = [
            "lekwati.com"
            "thalheim.io"
          ];
          tags.host = "eve";
          record_type = "A";
        };

        x509_cert = [{
          sources = [
            # nginx
            "https://devkid.net:443"
            "https://thalheim.io:443"
            # squid
            "https://devkid.net:8889"
            # dovecot
            "tcp://imap.thalheim.io:993"
            "tcp://imap.devkid.net:993"
            #  postfix
            "tcp://mail.thalheim.io:465"
          ];
          tags.host = "eve";
        } {
          sources = [
            # nginx
            "https://prometheus.thalheim.io:443"
            "https://alertmanager.thalheim.io:443"
          ];
          tags.host = "eva";
        }];
      };
    };
  };
}
