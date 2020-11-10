{ config, ... }:
{
  sops.secrets.promtail-password.owner = "promtail";
  systemd.services.promtail.serviceConfig.SupplementaryGroups = [ "keys" ];

  services.promtail = {
    enable = true;
    configuration = {
      server.http_listen_port = 9080;
      server.grpc_listen_port = 0;

      clients = [{
        basic_auth.username = "promtail";
        basic_auth.password_file = config.sops.secrets.promtail-password.path;
        url = "http://rock.r/loki/api/v1/push";
      }];

      scrape_configs = [{
        job_name = "journal";
        journal.max_age = "12h";
        journal.labels.job = "systemd-journal";
        relabel_configs = [{
          source_labels = [ "__journal__systemd_unit" ];
          target_label = "unit";
        }];
      }];
    };
  };
}
