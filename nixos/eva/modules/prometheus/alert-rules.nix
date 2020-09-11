{ lib }:

let
  deviceFilter = ''fstype!="ramfs",device!="rpc_pipefs",device!="lxcfs",device!="nsfs",device!="borgfs"'';
in lib.mapAttrsToList (name: opts: {
  alert = name;
  expr = opts.condition;
  for = opts.time or "2m";
  labels = {};
  annotations = {
    summary = opts.summary;
    description = opts.description;
  };
}) {
  #node_hwmon_temp = {
  #  condition = "node_hwmon_temp_celsius > node_hwmon_temp_crit_celsius*0.9 OR node_hwmon_temp_celsius > node_hwmon_temp_max_celsius*0.95";
  #  time = "5m";
  #  summary = "{{$labels.alias}}: Sensor {{$labels.sensor}}/{{$labels.chip}} temp is high: {{$value}} ";
  #  description = "{{$labels.alias}} reports hwmon sensor {{$labels.sensor}}/{{$labels.chip}} temperature value is nearly critical: {{$value}}";
  #};
  #node_conntrack_limit = {
  #  condition  = "node_nf_conntrack_entries_limit - node_nf_conntrack_entries < 1000";
  #  time = "5m";
  #  summary = "{{$labels.alias}}: Number of tracked connections high";
  #  description = "{{$labels.alias}} has only {{$value}} free slots for connection tracking available.";
  #};
  filesystem_full_80percent = {
    condition = "disk_used_percent >= 80";
    time = "10m";
    summary = "{{$labels.instance}}: Filesystem is running out of space soon.";
    description = "{{$labels.instance}} device {{$labels.device}} on {{$labels.path}} got less than 20% space left on its filesystem.";
  };

  filesystem_full_in_1d = {
    condition = "predict_linear(disk_free[1d], 24*3600) <= 0";
    time = "1h";
    summary = "{{$labels.instance}}: Filesystem is running out of space in one day.";
    description = "{{$labels.instance}} device {{$labels.device}} on {{$labels.path}} is running out of space in approx. 1 day";
  };

  inodes_full_in_1d = {
    condition = "predict_linear(disk_inodes_free[1d], 24*3600) < 0";
    time = "1h";
    summary = "{{$labels.instance}}: Filesystem is running out of inodes in one day.";
    description = "{{$labels.instance}} device {{$labels.device}} on {{$labels.path}} is running out of inodes in approx. 1 day";
  };

  swap_using_30percent = {
    condition = "mem_swap_total - (mem_swap_cached + mem_swap_free) > mem_swap_total * 0.3";
    time = "30m";
    summary = "{{$labels.instance}}: Using more than 30% of its swap.";
    description = "{{$labels.instance}} is using 30% of its swap space for at least 30 minutes.";
  };

  systemd_service_failed = {
    condition = ''systemd_units_active_code == 3'';
    summary = "{{$labels.instance}}: Service {{$labels.name}} failed to start.";
    description = "{{$labels.instance}} failed to (re)start service {{$labels.name}}.";
  };
  ram_using_90percent = {
    condition =  "mem_buffered + mem_free + mem_cached < mem_total * 0.1";
    time = "1h";
    summary = "{{$labels.instance}}: Using lots of RAM.";
    description = "{{$labels.instance}} is using at least 90% of its RAM for at least 1 hour.";
  };
  load15 = {
    condition = ''system_load15 / on(instance) count(system_n_cpus) by (instance) >= 1.0'';
    time = "10m";
    summary = "{{$labels.instance}}: Running on high load: {{$value}}";
    description = "{{$labels.instance}} is running with load15 > 1 for at least 5 minutes: {{$value}}";
  };
  reboot = {
    condition = "system_uptime < 300";
    summary = "{{$labels.alias}}: Reboot";
    description = "{{$labels.alias}} just rebooted.";
  };
  uptime = {
    condition = "system_uptime > 2592000";
    summary = "{{$labels.alias}}: Uptime monster";
    description = "{{$labels.alias}} has been up for more than 30 days.";
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
  #  summary = "{{$labels.alias}}: Mail exporter is down.";
  #  description = "Mail exporter on {{$labels.alias}} hasn't been responding more than 2 minutes.";
  #};
  #mail_delivery_unsuccessful = {
  #  condition = "mail_deliver_success == 0";
  #  summary = "{{$labels.alias}}: Mail delivery unsuccessful";
  #  description = "{{$labels.alias}}: Mail delivery unsuccessful";
  #};
  #mail_delivery_late = {
  #  condition = "increase(mail_late_mails_total[1h]) >= 1";
  #  summary = "{{$labels.alias}}: Mail delivery late";
  #  description = "{{$labels.alias}}: Mail delivery late";
  #};
  #mail_send_fails = {
  #  condition = "increase(mail_send_fails_total[1h]) >= 1";
  #  summary = "{{$labels.alias}}: Mail send failed";
  #  description = "{{$labels.alias}}: Mail send failed";
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

  alerts_silences_changed = {
    condition = ''abs(delta(alertmanager_silences{state="active"}[1h])) >= 1'';
    summary = "alertmanager: number of active silences has changed: {{$value}}";
    description = "alertmanager: number of active silences has changed: {{$value}}";
  };
}
