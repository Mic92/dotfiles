{ pkgs, ... }: {
  imports = [
    ../../modules/netdata.nix
  ];

  services.netdata.python.extraPackages = ps: [
    ps.psycopg2 ps.docker ps.dnspython
  ];

  services.netdata.stream.role = "master";

  krebs.secret.files.netdata-pushover = {
    path = "/etc/netdata/health_alarm_notify.conf";
    owner = "netdata";
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
