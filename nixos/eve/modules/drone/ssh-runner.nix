{
  pkgs,
  config,
  ...
}: {
  systemd.services.drone-runner-ssh = {
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      EnvironmentFile = [
        config.sops.secrets.drone.path
      ];
      Environment = [
        "DRONE_RUNNER_CAPACITY=10"
        "CLIENT_DRONE_RPC_HOST=127.0.0.1:3030"
      ];
      ExecStart = "${config.nur.repos.mic92.drone-runner-ssh}/bin/drone-runner-ssh";
      User = "drone-runner-ssh";
      Group = "drone-runner-ssh";
    };
  };

  users.users.drone-runner-ssh = {
    isSystemUser = true;
    group = "drone-runner-ssh";
  };

  users.groups.drone-runner-ssh = {};

  sops.secrets.drone = {};
}
