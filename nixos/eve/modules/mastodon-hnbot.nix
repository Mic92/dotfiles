{ pkgs
, config
, inputs
, ...
}: {
  systemd.services.mastodon-hnbot = {
    path = [
      inputs.nur-packages.packages.${pkgs.hostPlatform.system}.mastodon-hnbot
    ];
    script = ''
      exec hnbot \
        --points 50 \
        https://toot.matereal.eu \
        joerg.hackernews50@thalheim.io \
        "$(cat ${config.sops.secrets.hnbot-password.path})"
    '';
    serviceConfig = {
      Type = "oneshot";
      WorkingDirectory = [
        "/var/lib/mastodon-hnbot"
      ];
      ExecStopPost = "+${
        pkgs.writeShellScript "update-health" ''
          cat > /var/log/telegraf/mastadon-hnbot <<EOF
          task,frequency=daily last_run=$(date +%s)i,state="$([[ $EXIT_CODE == exited ]] && echo ok || echo fail)"
          EOF
        ''
      }";
      StateDirectory = [ "mastodon-hnbot" ];
      User = "mastodon-hnbot";
    };
  };

  systemd.timers.mastodon-hnbot = {
    description = "Post hackernews posts to mastodon";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnUnitActiveSec = "5min";
      OnBootSec = "5min";
    };
  };

  users.users.mastodon-hnbot = {
    isSystemUser = true;
    createHome = true;
    home = "/var/lib/mastodon-hnbot";
    group = "mastodon-hnbot";
  };
  users.groups.mastodon-hnbot = { };

  sops.secrets.hnbot-password.owner = "mastodon-hnbot";
}
