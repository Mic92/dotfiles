{ pkgs, ... }: { 

  systemd.services.mastodon-hnbot = {
    path = [
      pkgs.nur.repos.mic92.mastodon-hnbot
    ];
    script = ''
      hnbot \
        --points 50 \
        https://toot.matereal.eu \
        joerg.hackernews50@thalheim.io \
        "$(cat /run/keys/hnbot-password)"
    '';
    serviceConfig = {
      Type = "oneshot";
      WorkingDirectory = [
        "/var/lib/mastodon-hnbot"
      ];
      SupplementaryGroups = [ "keys" ];
      StateDirectory = ["mastodon-hnbot"];
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
  users.groups.mastodon-hnbot = {};

  krops.secrets.files.hnbot-password.owner = "mastodon-hnbot";
}
