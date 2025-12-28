{ config, ... }:
{
  imports = [
    ./matrix-alertmanager.nix
    ./irc-alertmanager.nix
    ./rules.nix
  ];

  services.prometheus = {
    webExternalUrl = "https://prometheus.thalheim.io";
    extraFlags = [ "--storage.tsdb.retention.time=30d" ];
    scrapeConfigs = [
      {
        job_name = "telegraf";
        scrape_interval = "60s";
        metrics_path = "/metrics";
        http_sd_configs = [
          {
            url = "https://tum-dse.github.io/doctor-cluster-config/telegraf.json";
            refresh_interval = "5m";
          }
        ];
        static_configs = [
          {
            targets = [
              "turingmachine.r:9273"
              "bernie.r:9273"
              #"rock.r:9273"
            ];
            labels.type = "mobile";
          }
          {
            targets = [
              "eva.r:9273"
              "eve.r:9273"
              "blob64.r:9273"
              "matchbox.r:9273"
              "alertmanager.r:80"
              "prometheus.r:80"
              #"rock.r:9273"
            ];
          }
          {
            targets = [
              "prism.r:9273"
              "neoprism.r:9273"
              "gum.r:9273"
              # Down forever
              # "kelle.r:9273"
            ];

            labels.org = "krebs";
          }
          {
            targets = [ "clan.lol:9273" ];
            labels.org = "clan-lol";
          }
          {
            targets = [ "[2a01:4f9:c012:8178::1]:9273" ];
            labels.org = "nixos-wiki";
          }
          #{
          #  targets = [
          #    "[fc9f:379f:7191:058f:82a1:0000:0000:0001]:9273" # nas
          #  ];
          #  labels.org = "dave";
          #}
        ];
      }
      #{
      #  metrics_path = "/prometheus";
      #  # https://api.fly.io/prometheus/
      #}
      {
        job_name = "gitea";
        scrape_interval = "60s";
        metrics_path = "/metrics";

        scheme = "https";
        static_configs = [ { targets = [ "git.thalheim.io:443" ]; } ];
      }
    ];
    alertmanagers = [ { static_configs = [ { targets = [ "localhost:9093" ]; } ]; } ];
  };
  services.prometheus.alertmanager = {
    enable = true;
    environmentFile = config.sops.secrets.alertmanager.path;
    webExternalUrl = "https://alertmanager.thalheim.io";
    listenAddress = "[::1]";
    configuration = {
      global = {
        # The smarthost and SMTP sender used for mail notifications.
        smtp_smarthost = "mail.thalheim.io:587";
        smtp_from = "alertmanager@thalheim.io";
        smtp_auth_username = "alertmanager@thalheim.io";
        smtp_auth_password = "$SMTP_PASSWORD";
      };
      route = {
        receiver = "default";
        routes = [
          {
            group_by = [ "host" ];
            match_re.org = "krebs";
            group_wait = "5m";
            group_interval = "5m";
            repeat_interval = "4h";
            receiver = "krebs";
          }
          {
            group_by = [ "host" ];
            match_re.org = "nixos-wiki";
            group_wait = "5m";
            group_interval = "5m";
            repeat_interval = "4h";
            receiver = "nixos-wiki";
          }
          {
            group_by = [ "host" ];
            match_re.org = "clan-lol";
            group_wait = "5m";
            group_interval = "5m";
            repeat_interval = "4h";
            receiver = "clan-lol";
          }
          {
            group_by = [ "host" ];
            match_re.org = "dave";
            group_wait = "5m";
            group_interval = "5m";
            repeat_interval = "4h";
            receiver = "dave";
          }
          {
            group_by = [ "host" ];
            group_wait = "30s";
            group_interval = "2m";
            repeat_interval = "2h";
            receiver = "all";
          }
        ];
      };
      receivers = [
        {
          name = "krebs";
          webhook_configs = [
            {
              url = "http://127.0.0.1:9223/";
              max_alerts = 5;
            }
          ];
        }
        {
          name = "numtide";
          webhook_configs = [
            # TODO
            #{
            #  send_resolved = true;
            #  url = "https://chat.ntd.one/plugins/alertmanager/api/webhook?token='xxxxxxxxxxxxxxxxxxx-yyyyyyy'";
            #}
          ];
        }
        {
          name = "nixos-wiki";
          webhook_configs = [
            {
              url = "http://localhost:9088/alert";
              max_alerts = 5;
            }
          ];
        }
        {
          name = "clan-lol";
          webhook_configs = [
            # TODO
            #{
            #  url = "http://localhost:4050/services/hooks/YWxlcnRtYW5hZ2VyX3NlcnZpY2U";
            #  max_alerts = 5;
            #}
          ];
        }
        {
          name = "dave";
          telegram_configs = [
            {
              chat_id = 42927997;
              bot_token = "$TELEGRAM_BOT_TOKEN";
            }
          ];
        }
        {
          name = "all";
          # pushover_configs = [
          #   {
          #     user_key = "$PUSHOVER_USER_KEY";
          #     token = "$PUSHOVER_TOKEN";
          #     priority = "0";
          #   }
          # ];
        }
        { name = "default"; }
      ];
    };
  };
}
