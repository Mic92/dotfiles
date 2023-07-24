{ config
, lib
, pkgs
, ...
}:
let
  irc-alerts = pkgs.stdenv.mkDerivation {
    name = "irc-alerts";
    src = ./irc-alerts.py;
    dontUnpack = true;
    buildInputs = [ pkgs.python3 ];
    installPhase = ''
      install -D -m755 $src $out/bin/irc-alerts
    '';
  };
in
{
  sops.secrets.alertmanager = { };
  sops.secrets.prometheus-irc-password = { };
  sops.secrets.hass-token.owner = "prometheus";

  services.prometheus = {
    enable = true;
    # checks fail because of missing secrets in the sandbox
    checkConfig = false;
    ruleFiles = [
      (pkgs.writeText "prometheus-rules.yml" (builtins.toJSON {
        groups = [
          {
            name = "alerting-rules";
            rules = import ./alert-rules.nix { inherit lib; };
          }
        ];
      }))
    ];
    webExternalUrl = "https://prometheus.thalheim.io";
    scrapeConfigs = [
      {
        job_name = "telegraf";
        scrape_interval = "60s";
        metrics_path = "/metrics";
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
              "rauter.r:9273"
            ];
            # to make it compatible with the node-exporter dashboard
            labels.host = "rauter.r:9273";
          }
          {
            targets = [
              "prism.r:9273"
              "gum.r:9273"
              "kelle.r:9273"
            ];

            labels.org = "krebs";
          }
          {
            targets = [
              "web01.local:9273"
            ];

            labels.org = "clan-lol";
          }
          #{
          #  targets = [
          #    "dev1.numtide.com.r:9273"
          #  ];

          #  labels.org = "numtide";
          #}
          {
            targets = map (host: "${host}.r:9273") [
              # university
              "amy"
              "clara"
              "rose"

              "astrid"
              "dan"
              "mickey"
              "bill"
              "nardole"
              "yasmin"
              "ryan"
              "graham"

              "astrid"
              "dan"
              "mickey"

              "jackson"
              "christina"
              "adelaide"
              "wilfred"
              "river"
              "jack"

              "ruby"
            ];

            labels.org = "uni";
          }
          {
            targets = [
              "build01.nix-community.org:9273"
              "build02.nix-community.org:9273"
              "build03.nix-community.org:9273"
              "build04.nix-community.org:9273"
              "darwin02.nix-community.org:9273"
              "darwin03.nix-community.org:9273"
              "web01.nix-community.org:9273"
            ];
            labels.org = "nix-community";
          }
        ];
      }
      {
        job_name = "homeassistant";
        scrape_interval = "60s";
        metrics_path = "/api/prometheus";

        authorization.credentials_file = config.sops.secrets.hass-token.path;

        scheme = "https";
        static_configs = [
          {
            targets = [
              "hass.thalheim.io:443"
            ];
          }
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
        static_configs = [
          {
            targets = [
              "git.thalheim.io:443"
            ];
          }
        ];
      }
    ];
    alertmanagers = [
      {
        static_configs = [
          {
            targets = [ "localhost:9093" ];
          }
        ];
      }
    ];
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
            match_re.org = "nix-community";
            group_wait = "5m";
            group_interval = "5m";
            repeat_interval = "4h";
            receiver = "nix-community";
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
        #{
        #  name = "numtide";
        #  slack_configs = [
        #    {
        #      token = "$SLACK_TOKEN";
        #      api_url = "https://";
        #    }
        #  ];
        #}
        {
          name = "nix-community";
          webhook_configs = [
            {
              url = "http://localhost:4050/services/hooks/YWxlcnRtYW5hZ2VyX3NlcnZpY2U";
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
          name = "all";
          pushover_configs = [
            {
              user_key = "$PUSHOVER_USER_KEY";
              token = "$PUSHOVER_TOKEN";
              priority = "0";
            }
          ];
        }
        {
          name = "default";
        }
      ];
    };
  };

  systemd.sockets =
    lib.mapAttrs'
      (name: opts:
        lib.nameValuePair "irc-alerts-${name}" {
          description = "Receive http hook and send irc message for ${name}";
          wantedBy = [ "sockets.target" ];
          listenStreams = [ "[::]:${builtins.toString opts.port}" ];
        })
      {
        krebs.port = 9223;
      };

  systemd.services =
    lib.mapAttrs'
      (name: opts:
        let
          serviceName = "irc-alerts-${name}";
          hasPassword = opts.passwordFile or null != null;
        in
        lib.nameValuePair serviceName {
          description = "Receive http hook and send irc message for ${name}";
          requires = [ "irc-alerts-${name}.socket" ];
          serviceConfig =
            {
              Environment =
                [
                  "IRC_URL=${opts.url}"
                ]
                ++ lib.optional hasPassword "IRC_PASSWORD_FILE=/run/${serviceName}/password";
              DynamicUser = true;
              User = serviceName;
              ExecStart = "${irc-alerts}/bin/irc-alerts";
            }
            // lib.optionalAttrs hasPassword {
              PermissionsStartOnly = true;
              ExecStartPre =
                "${pkgs.coreutils}/bin/install -m400 "
                + "-o ${serviceName} -g ${serviceName} "
                + "${config.sops.secrets.prometheus-irc-password.path} "
                + "/run/${serviceName}/password";
              RuntimeDirectory = serviceName;
            };
        })
      {
        krebs.url = "irc://prometheus@irc.r:6667/#xxx";
      };
}
