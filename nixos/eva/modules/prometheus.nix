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
      job_name = "telegraf";
      scrape_interval = "60s";
      metrics_path = "/metrics";
      static_configs = [{
        targets = [
          "eva.r:9273"
          "eve.r:9273"
        ];
      }];
    } {
      job_name = "healtchecks";
      scrape_interval = "60s";
      scheme = "https";

      metrics_path = "/projects/19949858-209b-4d47-99f3-ca27e9b0dd96/metrics/-YJxfazBu__IIQmIyze5_SHKTxtJg0PR";
      static_configs = [{
        targets = [ "healthchecks.io" ];
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
        pushover_configs = [{
          user_key = "$PUSHOVER_USER_KEY";
          token = "$PUSHOVER_TOKEN";
        }];
      } {
        name = "default";
      }];
    };
  };
}
