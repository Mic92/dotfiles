{ pkgs, ... }: {
  imports = [
    ../../modules/netdata.nix
  ];
  systemd.services.netdata = {
    serviceConfig.Environment="PYTHONPATH=${pkgs.netdata}/libexec/netdata/python.d/python_modules";
    path = with pkgs; [
      (python3.withPackages (ps: [ ps.psycopg2 ps.docker ]))
    ];
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
