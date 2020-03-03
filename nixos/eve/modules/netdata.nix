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

  services.netdata.python.extraPackages = ps: [
    # postgresql
    ps.psycopg2
    ps.docker
    ps.dnspython
    # tor
    ps.stem
    ps.ldap
  ];

  services.netdata.stream.role = "master";

  krops.secrets.netdata-pushover = {
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

  services.icinga2.extraConfig = ''
    apply Service "NETDATA v4 (eve)" {
      import "eve-http4-service"
      vars.http_vhost = "netdata.thalheim.io"
      vars.http_expect_body_regex = "netdata dashboard"
      assign where host.name == "eve.thalheim.io"
    }

    apply Service "NETDATA v6 (eve)" {
      import "eve-http6-service"
      vars.http_vhost = "netdata.thalheim.io"
      vars.http_expect_body_regex = "netdata dashboard"
      assign where host.name == "eve.thalheim.io"
    }

    apply Service "NETDATA CERTIFICATE (eve)" {
      import "eve-http-service"
      vars.http_vhost= "netdata.thalheim.io"
      vars.http_certificate = "30"
      assign where host.name == "eve.thalheim.io"
    }
  '';
}
