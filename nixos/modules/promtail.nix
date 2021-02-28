{ config, ... }:
{
  sops.secrets.promtail-password = {
    owner = "promtail";
    sopsFile = ../secrets/secrets.yaml;
  };
  systemd.services.promtail.serviceConfig.SupplementaryGroups = [ "keys" ];

  services.promtail = {
    enable = true;
    configuration = {
      server.http_listen_port = 9080;
      server.grpc_listen_port = 0;

      clients = [{
        basic_auth.username = "promtail@thalheim.io";
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
        pipeline_stages = [
          {
            json.expressions = {
              transport = "_TRANSPORT";
              unit = "_SYSTEMD_UNIT";
              msg = "MESSAGE";
              coredump_cgroup = "COREDUMP_CGROUP";
              coredump_exe = "COREDUMP_EXE";
              coredump_cmdline = "COREDUMP_CMDLINE";
              coredump_uid = "COREDUMP_UID";
              coredump_gid = "COREDUMP_GID";
            };
          }
          {
            # Set the unit (defaulting to the transport like audit and kernel)
            template = {
              source = "unit";
              template = "{{if .unit}}{{.unit}}{{else}}{{.transport}}{{end}}";
            };
          }
          {
            regex = {
              expression = "(?P<coredump_unit>[^/]+)$";
              source = "coredump_cgroup";
            };
          }
          {
            template = {
              source = "msg";
              # FIXME would be cleaner to have this in a match block, but could not get it to work
              template = "{{if .coredump_exe}}{{.coredump_exe}} core dumped (user: {{.coredump_uid}}/{{.coredump_gid}}, command: {{.coredump_cmdline}}){{else}}{{.msg}}{{end}}";
            };
          }
          {
            labels.coredump_unit = "coredump_unit";
          }
          {
            # Normalize session IDs (session-1234.scope -> session.scope) to limit number of label values
            replace = {
              source = "unit";
              expression = "^(session-\\d+.scope)$";
              replace = "session.scope";
            };
          }
          {
            labels.unit = "unit";
          }
          {
            # Write the proper message instead of JSON
            output.source = "msg";
          }
        ];
        relabel_configs = [{
          source_labels = [ "__journal__hostname" ];
          target_label = "host";
        }];
      }];
    };
  };
}
