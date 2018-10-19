{ pkgs, ... }: {
  imports = [
    ../../modules/netdata.nix
  ];
  systemd.services.netdata = {
    path = with pkgs; [
      (python3.withPackages (ps: [ ps.psycopg2 ps.docker ps.dnspython ]))
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
