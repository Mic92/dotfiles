{ pkgs, ... }:
{
  imports = [
    ../../modules/netdata
  ];

  nixpkgs.config.packageOverrides = pkgs: {
    netdata = pkgs.netdata.overrideAttrs (old: {
      patches = (old.patches or []) ++ [
        (pkgs.fetchpatch {
          url = "https://github.com/Mic92/netdata/commit/47ddf40f7e6cd32af6da11ad7dbc77433ddf08b8.patch";
          sha256 = "0qc9r3sxyhb2hbi5s98d5c77br6hsic2gm010ra80d2d6lyjvfaw";
        })
      ];
    });
  };

  services.netdata = {
    config.global."memory mode" = "save";

    python.extraPackages = ps: [
      # postgresql
      ps.psycopg2
      ps.docker
      ps.dnspython
      # tor
      ps.stem
      ps.ldap
    ];

    stream.role = "master";
  };

  sops.secrets.netdata-pushover = {
    path = "/etc/netdata/health_alarm_notify.conf";
    owner = "netdata";
  };

  services.nginx.virtualHosts."netdata.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/".extraConfig = ''
      proxy_pass http://localhost:19999;
    '';
  };
}
