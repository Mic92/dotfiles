{ lib, ... }: {
  srvos.prometheus = {
    ruleGroups.srvosAlerts.alertRules =
      (lib.genAttrs [
        "borgbackup-job-turingmachine.service"
        "borgbackup-job-eve.service"
        "borgbackup-job-matchbox.service"
        "borgbackup-job-nfs-home.service"
        "borgbackup-job-nfs-share.service"
      ]
        (name: {
          expr = ''absent_over_time(task_last_run{name="${name}"}[1d])'';
          annotations.description = "status of ${name} is unknown: no data for a day";
        })) //
      (lib.genAttrs [
        "syncoid-home"
        "syncoid-share"
      ]
        (name: {
          expr = ''absent_over_time(task_last_run{name="${name}"}[10m])'';
          annotations.description = "status of ${name} is unknown: no data for 10 minutes";
        })) //
      {

        Homeassistant = {
          expr = ''homeassistant_entity_available{domain="persistent_notification", entity!~"persistent_notification.http_login|persistent_notification.recorder_database_migration"} >= 0'';
          annotations.description = "homeassistant notification {{$labels.entity}} ({{$labels.friendly_name}}): {{$value}}";
        };

        Filesystem80percentFull = lib.mkForce {
          expr = ''disk_used_percent{mode!="ro", org!="krebs"} >= 80'';
          for = "10m";
          annotations.description = "{{$labels.instance}} device {{$labels.device}} on {{$labels.path}} got less than 20% space left on its filesystem";
        };

        FilesystemFullKrebs = {
          expr = ''disk_used_percent{mode!="ro", org="krebs"} >= 95'';
          for = "10m";
          annotations.description = "{{$labels.instance}} device {{$labels.device}} on {{$labels.path}} got less than 5% space left on its filesystem";
        };

        Gitea = {
          expr = ''rate(promhttp_metric_handler_requests_total{job="gitea", code="500"}[5m]) > 3'';
          annotations.description = "{{$labels.instance}}: gitea instances error rate went up: {{$value}} errors in 5 minutes";
        };

        # too scared to upgrade matchbox
        Uptime.expr = lib.mkForce ''system_uptime {host!="matchbox"} > 2592000'';

        PublicRunnerActionOnline = {
          expr = ''count(http_busy{name=~"runner.*", status="online"}) < 2'';
          annotations.description = "{{$labels.instance}}: There are no public github action runner registerd with github (see https://github.com/organizations/ls1-sys-prog-course/settings/actions)";
        };

        InternalRunnerActionOnline = {
          expr = ''count(http_busy{name=~"internal-runner.*", status="online"}) < 1'';
          annotations.description = "{{$labels.instance}}: There are no interal github action runner registerd with github (see https://github.com/organizations/ls1-sys-prog-course-internal/settings/actions)";
        };

        # we don't have this course this semester
        #CloudlabGithubActionRunner = {
        #  expr = ''count(kubernetes_pod_container_state_code{pod_name=~"cloudlab-runner-deployment.*", state="running",container_name="runner"}) == 0'';
        #  annotations.description = "{{$labels.instance}}: There are no github action runner {{$value}} for (https://github.com/organizations/ls1-sys-prog-course/settings/actions)";
        #};

        #CloudlabGithubActionRunnerPresent = {
        #  expr = ''absent_over_time(kubernetes_pod_container_state_code{pod_name=~"cloudlab-runner-deployment.*", state="running",container_name="runner"}[10m])'';
        #  annotations.description = "status of public github action runner is unknown: no data for 10 minutes";
        #};

        InternalGithubActionRunner = {
          expr = ''count(kubernetes_pod_container_state_code{pod_name=~"internal-runner-deployment.*", state="running",container_name="runner"}) == 0'';
          annotations.description = "{{$labels.instance}}: There are no github action runner {{$value}} for (https://github.com/organizations/ls1-sys-prog-course-internal/settings/actions)";
        };

        InternalGithubActionRunnerPresent = {
          expr = ''absent_over_time(kubernetes_pod_container_state_code{pod_name=~"internal-runner-deployment.*", state="running",container_name="runner"}[10m])'';
          annotations.description = "status of internal github action runner is unknown: no data for 10 minutes";
        };

        NavidromeNotEnoughAlbums = {
          expr = ''http_navidrome_album_count != 1'';
          annotations.description = "navidrome: not enough albums as expected: {{$value}}";
        };

        ## Overrides from srvos:

        NixpkgsOutOfDate.expr = lib.mkForce ''(time() - flake_input_last_modified{input="nixpkgs", host!="matchbox"}) / (60 * 60 * 24) > 7'';
      };
  };
}
