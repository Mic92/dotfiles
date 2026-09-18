{ config, lib, ... }:
let
  am = "http://[::1]:9093";
  vm = "http://127.0.0.1:8428";
  vlogs = "http://127.0.0.1:9428";
in
{
  # roles-prometheus is only imported for srvos.prometheus.ruleGroups
  services.prometheus.enable = false;

  srvos.prometheus.ruleGroups.srvosAlerts.alertRules = {
    PrometheusNotConnectedToAlertmanager.enable = false;
    PrometheusRuleEvaluationFailures.enable = false;
    PrometheusTemplateExpansionFailures.enable = false;
    PromtailRequestsErrors.enable = false;
    PromtailFileLagging.enable = false;
    MonitoringTooManyRestarts.expr = lib.mkForce ''changes(process_start_time_seconds{job=~"victoria-metrics|vmalert.*|alertmanager|telegraf"}[15m]) > 2'';

    VmalertRuleErrors = {
      expr = "increase(vmalert_execution_errors_total[5m]) > 0";
      annotations.description = "vmalert rule {{$labels.alertname}}{{$labels.recording}} in group {{$labels.group}} fails to evaluate";
    };
    VmalertAlertmanagerErrors = {
      expr = "increase(vmalert_alerts_send_errors_total[5m]) > 0";
      annotations.description = "vmalert cannot deliver alerts to {{$labels.addr}}";
    };
  };

  services.vmalert.instances = {
    metrics = {
      enable = true;
      settings = {
        "datasource.url" = vm;
        "notifier.url" = [ am ];
        "remoteWrite.url" = vm;
        "remoteRead.url" = vm;
        "httpListenAddr" = "127.0.0.1:8880";
        "external.url" = "https://prometheus.thalheim.io";
        "external.alert.source" = "vmui/#/?g0.expr={{.Expr|queryEscape}}";
        rule = config.services.prometheus.ruleFiles;
      };
    };

    logs = {
      enable = true;
      settings = {
        "datasource.url" = vlogs;
        "notifier.url" = [ am ];
        "remoteWrite.url" = vm;
        "remoteRead.url" = vm;
        "httpListenAddr" = "127.0.0.1:8881";
        "rule.defaultRuleType" = "vlogs";
        "external.url" = "https://prometheus.thalheim.io";
      };
      rules.groups = [
        {
          name = "logs";
          type = "vlogs";
          interval = "1m";
          rules = [
            {
              alert = "Coredumps";
              # filter out failed CI runners, users or nix build sandboxes
              expr = ''_time:10m unit:~"^systemd-coredump" "core dumped" !~"(/runner/_work|/home|/build|/scratch)" | stats by (host) count() as coredumps | filter coredumps:>0'';
              annotations.description = "{{ $labels.host }} core dumped {{ $value }}x in last 10min.";
            }
            {
              # host logged within 15m but not within 5m: shipper died
              alert = "LogIngestStalled";
              expr = ''_time:15m job:="systemd-journal" | stats by (host) count() as total, count() if (_time:5m) as recent | filter total:>0 recent:=0 | fields host, recent'';
              for = "5m";
              annotations.description = "{{ $labels.host }} has not shipped any journal logs in the last 5 minutes (but did in the last 15m).";
            }
            {
              record = "logs:sshd_invalid_user:rate5m";
              expr = ''_time:5m job:="systemd-journal" unit:="sshd.service" "Invalid user" | stats by (host) count() as n | math n / 300 as rate | fields host, rate'';
            }
          ];
        }
      ];
    };
  };

  services.victoriametrics.prometheusConfig.scrape_configs = [
    {
      job_name = "vmalert";
      scrape_interval = "60s";
      static_configs = [
        {
          targets = [
            "127.0.0.1:8880"
            "127.0.0.1:8881"
          ];
          labels.host = "eva";
        }
      ];
    }
  ];
}
