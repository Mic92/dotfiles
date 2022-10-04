{
  pkgs,
  config,
  lib,
  ...
}: let
  droneserver = config.users.users.droneserver.name;
in {
  nix.settings.allowed-users = ["drone-runner-exec"];
  systemd.services.drone-runner-exec = {
    wantedBy = ["multi-user.target"];
    # might break deployment
    restartIfChanged = false;
    confinement.enable = true;
    confinement.packages = [
      pkgs.git
      pkgs.gnutar
      pkgs.bash
      pkgs.nix
      pkgs.gzip
    ];
    path = [
      pkgs.git
      pkgs.gnutar
      pkgs.bash
      pkgs.nix
      pkgs.gzip
    ];
    serviceConfig = {
      Environment = [
        "DRONE_RUNNER_CAPACITY=10"
        "CLIENT_DRONE_RPC_HOST=127.0.0.1:3030"
        "NIX_REMOTE=daemon"
        "PAGER=cat"
      ];
      BindPaths = [
        "/nix/var/nix/daemon-socket/socket"
        "/run/nscd/socket"
        "/var/lib/drone"
      ];
      BindReadOnlyPaths = [
        "/etc/passwd:/etc/passwd"
        "/etc/group:/etc/group"
        "/nix/var/nix/profiles/system/etc/nix:/etc/nix"
        "${config.environment.etc."ssl/certs/ca-certificates.crt".source}:/etc/ssl/certs/ca-certificates.crt"
        "${config.environment.etc."ssh/ssh_known_hosts".source}:/etc/ssh/ssh_known_hosts"
        "${
          builtins.toFile "ssh_config" ''
            Host eve.thalheim.io
              ForwardAgent yes
          ''
        }:/etc/ssh/ssh_config"
        "/etc/machine-id"
        # channels are dynamic paths in the nix store, therefore we need to bind mount the whole thing
        "/nix/"
      ];
      EnvironmentFile = [
        config.sops.secrets.drone.path
      ];
      ExecStart = "${pkgs.drone-runner-exec}/bin/drone-runner-exec";
      User = "drone-runner-exec";
      Group = "drone-runner-exec";
    };
  };

  systemd.tmpfiles.rules = [
    "d /var/lib/drone/ 0700 drone-runner-exec drone-runner-exec - -"
  ];

  users.users.drone-runner-exec = {
    isSystemUser = true;
    group = "drone-runner-exec";
  };
  users.groups.drone-runner-exec = {};
}
