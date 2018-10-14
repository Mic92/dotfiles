{ pkgs, ... }: {
  imports = [
    ../../modules/netdata.nix
  ];
  systemd.services.netdata = {
    serviceConfig.Environment="PYTHONPATH=${pkgs.netdata}/libexec/netdata/python.d/python_modules";
    path = with pkgs; [
      (python3.withPackages (ps: [ ps.psycopg2 ]))
      nodejs
    ];
  };
}
