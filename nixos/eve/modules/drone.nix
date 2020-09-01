{ pkgs, config, ... }:

let
  droneserver = config.users.users.droneserver.name;
  droneNet = "2a01:4f9:2b:1605:3::1/80";
  droneRetiolumNet = "42:0000:002b:1605:3::/80";
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

  systemd.services.netmap-docker-retiolum = {
    wantedBy = [ "multi-user.target" ];
    requires = [ "sys-devices-virtual-net-tinc.retiolum.device" ];
    after = [ "network.target" "sys-devices-virtual-net-tinc.retiolum.device" ];
    serviceConfig = let
      removeCommands = [
        "-${pkgs.iptables}/bin/ip6tables -t nat -D PREROUTING -i tinc.retiolum -d ${droneRetiolumNet} -j NETMAP --to ${droneNet}"
        "-${pkgs.iptables}/bin/ip6tables -t nat -D POSTROUTING -o tinc.retiolum -s ${droneNet} -j NETMAP --to ${droneRetiolumNet}"
      ];
    in {
      Type = "oneshot";
      RemainAfterExit  = true;
      ExecStart = removeCommands ++ [
        "${pkgs.iptables}/bin/ip6tables -t nat -A PREROUTING -i tinc.retiolum -d ${droneRetiolumNet} -j NETMAP --to ${droneNet}"
        "${pkgs.iptables}/bin/ip6tables -t nat -A POSTROUTING -o tinc.retiolum -s ${droneNet} -j NETMAP --to ${droneRetiolumNet}"
      ];
      ExecStop = removeCommands;
    };
  };

  systemd.services.drone-agent = {
    wantedBy = [ "multi-user.target" ];
    # might break deployment
    restartIfChanged = false;
    preStart = ''
        export PATH=${pkgs.docker}/bin:$PATH
        docker network rm drone || true
        while ! docker network ls | grep -q drone; do
          docker network create \
            --driver=bridge \
            --subnet=172.28.0.0/16 \
            --gateway=172.28.0.1 \
            --subnet ${droneNet} \
            --ipv6 drone
        done
    '';
    serviceConfig = {
      Environment = [
        "DRONE_SERVER_PORT=:3030"
        "DRONE_RUNNER_NETWORKS=drone"
        "DRONE_RUNNER_CAPACITY=10"
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
