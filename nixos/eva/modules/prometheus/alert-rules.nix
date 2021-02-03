{ lib }:

let
  # docker's filesystems disappear quickly, leading to false positives
  deviceFilter = ''path!~"^(/var/lib/docker|/nix/store).*"'';
in lib.mapAttrsToList (name: opts: {
  alert = name;
  expr = opts.condition;
  for = opts.time or "2m";
  labels = {};
  annotations = {
    summary = opts.summary;
    description = opts.description;
  };
}) ({
  filesystem_full_80percent = {
    condition = ''disk_used_percent{mode!="ro"} >= 80'';
    time = "10m";
    summary = "{{$labels.instance}}: Filesystem is running out of space soon.";
    description = "{{$labels.instance}} device {{$labels.device}} on {{$labels.path}} got less than 20% space left on its filesystem.";
  };

  daily_task_not_run = {
    # give 6 hours grace period
    # FIXME: convert borgbackup-matchbox to weekly task
    condition = ''time() - task_last_run{state="ok",frequency="daily",name!="borgbackup-matchbox"} > (24 + 6) * 60 * 60'';
    summary = "{{$labels.host}}: {{$labels.name}} was not run in the last 24h";
    description = "{{$labels.host}}: {{$labels.name}} was not run in the last 24h";
  };

  daily_task_failed = {
    condition = ''task_last_run{state="fail"}'';
    summary = "{{$labels.host}}: {{$labels.name}} failed to run";
    description = "{{$labels.host}}: {{$labels.name}} failed to run";
  };
} // (lib.genAttrs [
      "borgbackup-amy"
      "borgbackup-turingmachine"
      "borgbackup-eve"
      "borgbackup-datastore"
      "borgbackup-martha"
    ] (name: {
      condition = ''absent_over_time(task_last_run{name="${name}"}[1d])'';
      summary = "status of ${name} is unknown";
      description = "status of ${name} is unknown: no data for a day";
    }))
// {
  borgbackup_matchbox_not_run = {
    # give 6 hours grace period
    condition = ''time() - task_last_run{state="ok",frequency="daily",name="borgbackup-matchbox"} > 7 * 24 * 60 * 60'';
    summary = "{{$labels.host}}: {{$labels.name}} was not run in the last week";
    description = "{{$labels.host}}: {{$labels.name}} was not run in the last week";
  };
  borgbackup_matchbox = {
    condition = ''absent_over_time(task_last_run{name="borgbackup-matchbox"}[7d])'';
    summary = "status of borgbackup-matchbox is unknown";
    description = "status of borgbackup-matchbox is unknown: no data for a week";
  };
  filesystem_full_in_1d = {
    condition = "predict_linear(disk_free{${deviceFilter}}[1d], 24*3600) <= 0";
    time = "1h";
    summary = "{{$labels.host}}: Filesystem is running out of space in one day.";
    description = "{{$labels.host}} device {{$labels.device}} on {{$labels.path}} is running out of space in approx. 1 day";
  };

  inodes_full_in_1d = {
    condition = "predict_linear(disk_inodes_free{${deviceFilter}}[1d], 24*3600) < 0";
    time = "1h";
    summary = "{{$labels.host}}: Filesystem is running out of inodes in one day.";
    description = "{{$labels.host}} device {{$labels.device}} on {{$labels.path}} is running out of inodes in approx. 1 day";
  };

  swap_using_30percent = {
    condition = "mem_swap_total - (mem_swap_cached + mem_swap_free) > mem_swap_total * 0.3";
    time = "30m";
    summary = "{{$labels.host}}: Using more than 30% of its swap.";
    description = "{{$labels.host}} is using 30% of its swap space for at least 30 minutes.";
  };

  systemd_service_failed = {
    condition = ''systemd_units_active_code{name!="nixpkgs-update.service"} == 3'';
    summary = "{{$labels.host}}: Service {{$labels.name}} failed to start.";
    description = "{{$labels.host}} failed to (re)start service {{$labels.name}}.";
  };
  ram_using_90percent = {
    condition =  "mem_buffered + mem_free + mem_cached < mem_total * 0.1";
    time = "1h";
    summary = "{{$labels.host}}: Using lots of RAM.";
    description = "{{$labels.host}} is using at least 90% of its RAM for at least 1 hour.";
  };
  load15 = {
    condition = ''system_load15 / system_n_cpus{org!="nix-community"} >= 2.0'';
    time = "10m";
    summary = "{{$labels.host}}: Running on high load: {{$value}}";
    description = "{{$labels.host}} is running with load15 > 1 for at least 5 minutes: {{$value}}";
  };
  reboot = {
    condition = "system_uptime < 300";
    summary = "{{$labels.host}}: Reboot";
    description = "{{$labels.host}} just rebooted.";
  };
  uptime = {
    # too scared to upgrade matchbox
    condition = ''system_uptime {host!="matchbox"} > 2592000'';
    summary = "{{$labels.host}}: Uptime monster";
    description = "{{$labels.host}} has been up for more than 30 days.";
  };
  telegraf_down = {
    condition = ''min(up{job=~"telegraf"}) by (source, job, instance) == 0'';
    time = "3m";
    summary = "{{$labels.instance}}: {{$labels.job}} telegraf exporter from {{$labels.source}} is down.";
    description = "{{$labels.instance}}: {{$labels.job}} telegraf exporter from {{$labels.source}} is down.";
  };
  ping = {
    condition = "ping_result_code{type!='mobile'} == 1";
    summary = "{{$labels.url}}: ping from {{$labels.instance}} has failed!";
    description = "{{$labels.url}}: ping from {{$labels.instance}} has failed!";
  };
  ping_high_latency = {
    condition = "ping_average_response_ms{type!='mobile'} > 5000";
    summary = "{{$labels.instance}}: ping probe from {{$labels.source}} takes too long!";
    description = "{{$labels.instance}}: ping probe from {{$labels.source}} is encountering high latency!";
  };
  http =  {
    condition = "http_response_result_code != 0";
    summary = "{{$labels.server}} : http request failed: {{$labels.result}}";
    description = "{{$labels.server}} : http request failed from {{$labels.instance}}: {{$labels.result}}!";
  };
  http_match_failed = {
    condition = "http_response_response_string_match == 0";
    summary = "{{$labels.server}} : http body not as expected";
    description = "{{$labels.server}} : http body not as expected; status code: {{$labels.status_code}}!";
  };
  dns_query = {
    condition = "dns_query_result_code != 0";
    summary = "{{$labels.domain}} : dns query failed: {{$labels.result}}";
    description = "{{$labels.domain}} : could retrieve A record {{$labels.instance}} from server {{$labels.server}}: {{$labels.result}}!";
  };
  connection_failed = {
    condition = "net_response_result_code != 0";
    summary = "{{$labels.server}}: connection to {{$labels.port}}({{$labels.protocol}}) failed";
    description = "{{$labels.server}}: connection to {{$labels.port}}({{$labels.protocol}}) failed from {{$labels.instance}}";
  };
  healthchecks = {
    condition = "hc_check_up == 0";
    summary = "{{$labels.instance}}: healtcheck {{$labels.job}} fails!";
    description = "{{$labels.instance}}: healtcheck {{$labels.job}} fails!";
  };
  cert_expiry = {
    condition = "x509_cert_expiry < 7*24*3600";
    summary = "{{$labels.instance}}: TLS certificate from {{$labels.source}} is about to expire.";
    description = "{{$labels.instance}}: The TLS certificate from {{$labels.source}} will expire in less than 7 days: {{$value}}s";
  };
  # TODO
  #mail_down = {
  #  condition = ''up{job="mail"} == 0'';
  #  summary = "{{$labels.host}}: Mail exporter is down.";
  #  description = "Mail exporter on {{$labels.host}} hasn't been responding more than 2 minutes.";
  #};
  #mail_delivery_unsuccessful = {
  #  condition = "mail_deliver_success == 0";
  #  summary = "{{$labels.host}}: Mail delivery unsuccessful";
  #  description = "{{$labels.host}}: Mail delivery unsuccessful";
  #};
  #mail_delivery_late = {
  #  condition = "increase(mail_late_mails_total[1h]) >= 1";
  #  summary = "{{$labels.host}}: Mail delivery late";
  #  description = "{{$labels.host}}: Mail delivery late";
  #};
  #mail_send_fails = {
  #  condition = "increase(mail_send_fails_total[1h]) >= 1";
  #  summary = "{{$labels.host}}: Mail send failed";
  #  description = "{{$labels.host}}: Mail send failed";
  #};

  postfix_queue_length = {
    condition = "avg_over_time(postfix_queue_length[1h]) > 10";
    summary = "{{$labels.instance}}: mail queue is filling up: {{$value}}";
    description = "{{$labels.instance}}: postfix mail queue has undelivered {{$value}} items";
  };

  zfs_errors = {
    condition = "zfs_arcstats_l2_io_error + zfs_dmu_tx_error + zfs_arcstats_l2_writes_error > 0";
    summary = "{{$labels.instance}}: ZFS IO errors: {{$value}}";
    description = "{{$labels.instance}} reports: {{$value}} ZFS IO errors. Drive(s) are failing.";
  };

  smart_errors = {
    condition = "smart_device_health_ok != 1";
    summary = "{{$labels.instance}}: S.M.A.R.T health not ok";
    description = "{{$labels.instance}}: S.M.A.R.T reports: {{$labels.device}} ({{$labels.model}}) has errors.";
  };

  oom_kills = {
    condition = "increase(kernel_vmstat_oom_kill[5m]) > 0";
    summary = "{{$labels.instance}}: OOM kill detected";
    description = "{{$labels.instance}}: OOM kill detected";
  };

  ext4_errors = {
    condition = "ext4_errors_value > 0";
    summary = "{{$labels.instance}}: ext4 reports errors";
    description = "{{$labels.instance}}: ext4 has reported {{$value}} I/O errors: check /sys/fs/ext4/*/errors_count";
  };

  alerts_silences_changed = {
    condition = ''abs(delta(alertmanager_silences{state="active"}[1h])) >= 1'';
    summary = "alertmanager: number of active silences has changed: {{$value}}";
    description = "alertmanager: number of active silences has changed: {{$value}}";
  };
})
