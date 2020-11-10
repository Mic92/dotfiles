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
        journal = {
          json = true;
          max_age = "12h";
          labels.job = "systemd-journal";
        };
        pipeline_stages = [{
          json.expressions = {
            transport = "_TRANSPORT";
            unit = "_SYSTEMD_UNIT";
            msg = "MESSAGE";
          };
        } {
          # Set the unit (defaulting to the transport like audit and kernel)
          template = {
            source = "unit";
            template = "{{if .unit}}{{.unit}}{{else}}{{.transport}}{{end}}";
          };
        } {
          # Normalize session IDs (session-1234.scope -> session.scope) to limit number of label values
          replace = {
            source = "unit";
            expression = "^(session-\\d+.scope)$";
            replace = "session.scope";
          };
        } {
          # Write the unit label
          labels = { unit = "unit"; };
        } {
          # Write the proper message instead of JSON
          output.source = "msg";
        } {
          # Drop useless messages
          drop.expression = "xfs filesystem being remounted at [^ ]+ supports timestamps until 2038 \\(0x7fffffff\\)";
        }];
        relabel_configs = [{
          source_labels = [ "__journal__hostname" ];
          target_label = "nodename";
        }];
      }];
    };
  };
}
