{ pkgs, ... }:
let
  openttd-server = pkgs.stdenv.mkDerivation {
    name = "openttd";
    src = null;
    dontUnpack = true;
    buildInputs = [ pkgs.bash ];
    installPhase = ''
      install -D ${./server.sh} $out/bin/openttd;
    '';
  };
in
{
  users.users.openttd = {
    isSystemUser = true;
    home = "/var/lib/openttd";
    group = "openttd";
  };

  networking.firewall.allowedTCPPorts = [ 3979 ];
  networking.firewall.allowedUDPPorts = [ 3979 ];

  users.groups.openttd = { };

  systemd.services.openttd = {
    # start on demand
    wantedBy = [ ];
    path = [ pkgs.openttd ];
    serviceConfig = {
      ExecStart = "${openttd-server}/bin/openttd";
      StateDirectory = "openttd";
      User = "openttd";
    };
  };
}
