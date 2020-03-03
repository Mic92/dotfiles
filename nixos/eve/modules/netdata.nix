{ pkgs, ... }:
{
  imports = [
    ../../modules/netdata
  ];

  nixpkgs.config.packageOverrides = pkgs: {
    netdata = pkgs.netdata.overrideAttrs (old: {
      patches = (old.patches or []) ++ [
        (pkgs.fetchpatch {
          url = "https://github.com/Mic92/netdata/commit/a73ac203506a9cc623fb5ba3d7fc4b886609d638.patch";
          sha256 = "00a13mhwfwr4ia4fgq7zwpiadpzrvv7mrdfa8h6zgij4x02z8xi9";
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
