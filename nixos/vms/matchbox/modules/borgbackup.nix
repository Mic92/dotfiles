{ pkgs, ... }: let
  borgweb = with pkgs.python3.pkgs; buildPythonPackage rec {
    pname = "borgweb";
    version = "0.3.0";
    propagatedBuildInputs = [ flask ];
    buildInputs = [ setuptools_scm ];
    src = fetchPypi {
      inherit pname version;
      sha256 = "0pm6fcx9ixgzlyss0dj3yy716bixrcynszqhm7y19sifbr7n7wp3";
    };
  };
in {
  networking.firewall.allowedTCPPorts = [ 80 ];
  systemd.services.borgweb = {
    description = "Borgbackup web-interface";
    documentation = ["https://borgweb.readthedocs.io/en/latest/"];
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    path = [ pkgs.borgbackup pkgs.utillinux ];
    environment = {
      BORGWEB_CONFIG = "${../borg-configuration}";
      BORG_LOGGING_CONF = "${../borg-logging.conf}";
      BORG_UNKNOWN_UNENCRYPTED_REPO_ACCESS_IS_OK = "yes";
    };
    serviceConfig = {
      ExecStart = "${borgweb}/bin/borgweb";
      Restart = "on-failure";
      SupplementaryGroups = [ "keys" ];
    };
  };
  systemd.tmpfiles.rules = [
    "d /var/log/borgweb 0700 root root -"
  ];
}
