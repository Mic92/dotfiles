{ config, lib, pkgs, ... }:

{
  services.prometheus = {
    enable = true;
    ruleFiles = [(pkgs.writeText "prometheus-rules.yml" (builtins.toJSON {
      groups = [{
        name = "alerting-rules";
        rules = import ./alert-rules.nix { inherit lib; };
      }];
    }))];
    scrapeConfigs = [{
      job_name = "blackbox";
      scrape_interval = "60s";
      metrics_path = "/probe";
      params.module = [ "icmp_v4" ];
      static_configs = [{
        targets = [ "eve.thalheim.io" ];
        labels.source = "localhost";
      }];
      relabel_configs = [{
        source_labels = [ "__address__" ];
        regex = "(.*)(:80)?";
        target_label = "__param_target";
        replacement = ''''${1}'';
      } {
        source_labels = [ "__param_target" ];
        regex = "(.*)";
        target_label = "instance";
        replacement = ''''${1}'';
      } {
        source_labels = [];
        regex = ".*";
        target_label = "__address__";
        replacement = "localhost:9115";
      }];
    }];
    alertmanagers = [{
      static_configs = [{
        targets = [ "localhost:9093" ];
      }];
    }];
  };

  sops.secrets.alertmanager = {};

  services.prometheus.alertmanager = {
    enable = true;
    environmentFile = config.sops.secrets.alertmanager.path;
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
        routes = [{
          group_by = [ "instance" ];
          group_wait = "30s";
          group_interval = "2m";
          repeat_interval = "2h";
          receiver = "all";
        }];
      };
      receivers = [{
        name = "all";
        email_configs = [{
          to = "joerg@thalheim.io";
        }];
      } {
        name = "default";
      }];
    };
  };
}
