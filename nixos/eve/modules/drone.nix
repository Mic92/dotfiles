{ pkgs, config, ... }:

let
  droneserver = config.users.users.droneserver.name;
in {
  systemd.services.drone-server = {
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      EnvironmentFile = [
        config.sops.secrets.drone.path
      ];
      Environment = [
        "DRONE_DATABASE_DATASOURCE=postgres:///droneserver?host=/run/postgresql"
        "DRONE_DATABASE_DRIVER=postgres"
        "DRONE_SERVER_PORT=:3030"
        "DRONE_USER_CREATE=username:Mic92,admin:true"
      ];
      ExecStart = "${pkgs.drone}/bin/drone-server";
      User = droneserver;
      Group = droneserver;
    };
  };

  services.postgresql = {
    ensureDatabases = [ droneserver ];
    ensureUsers = [{
      name = droneserver;
      ensurePermissions = {
        "DATABASE ${droneserver}" = "ALL PRIVILEGES";
      };
    }];
  };

  services.nginx.virtualHosts."drone.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/".extraConfig = ''
      proxy_pass http://localhost:3030;
    '';
  };

  systemd.services.drone-agent = {
    wantedBy = [ "multi-user.target" ];
    preStart = ''
        export PATH=${pkgs.docker}/bin:$PATH
        docker network rm drone || true
        docker network rm drone-retiolum || true
        while ! docker network ls | grep -q drone; do
          docker network create \
            --driver=bridge \
            --subnet=172.28.0.0/16 \
            --gateway=172.28.0.1 \
            --subnet 2a01:4f9:2b:1605:3::1/80 \
            --ipv6 drone
          docker network create \
            --driver=bridge \
            --subnet 42:0000:3c46:70c7::/80 \
            --ipv6 drone-retiolum
        done
    '';
    serviceConfig = {
      Environment = [
        "DRONE_SERVER_PORT=:3030"
        "DRONE_RUNNER_NETWORKS=drone,drone-retiolum"
      ];
      EnvironmentFile = [ config.sops.secrets.drone.path ];
      ExecStart = "${pkgs.drone}/bin/drone-agent";
      User = "drone-agent";
      Group = "drone-agent";
      SupplementaryGroups = [ "docker" ];
      DynamicUser = true;
    };
  };

  users.users.droneserver = {
    isSystemUser = true;
    createHome = true;
    group = droneserver;
  };
  users.groups.droneserver = {};

  sops.secrets.drone = { };
}
