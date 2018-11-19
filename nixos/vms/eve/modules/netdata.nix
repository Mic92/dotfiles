{ pkgs, ... }: {
  imports = [
    ../../modules/netdata.nix
  ];

  services.netdata.python.extraPackages = ps: [
    ps.psycopg2 ps.docker ps.dnspython
  ];

  services.netdata.stream.role = "master";

  environment.etc."netdata/health_alarm_notify.conf".source = "/run/keys/netdata-pushover.conf";
  deployment.keys."netdata-pushover.conf" = {
    keyFile = ../secrets/netdata-pushover.conf;
    user = "netdata";
  };

  services.nginx = {
    virtualHosts."netdata.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      locations."/".extraConfig = ''
        proxy_pass http://localhost:19999;
      '';
    };
  };
}
