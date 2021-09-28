{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config;
in
{
   options.services.hydra = {
     admin = {
       user = mkOption {
         type = types.str;
         default = "admin";
         description = "The initial password for the Hydra admin account";
       };
       passwordFile = mkOption {
         type = types.nullOr types.str;
         default = null;
         description = "The initial password for the Hydra admin account";
       };
     };
  };
  config = {
    # Create a admin user and configure a declarative project
    systemd.services.hydra-post-init = {
      serviceConfig = {
        Type = "oneshot";
        TimeoutStartSec = "60";
      };
      wantedBy = [ "multi-user.target" ];
      after = [ "hydra-server.service" ];
      requires = [ "hydra-server.service" ];
      environment = {
        inherit (cfg.systemd.services.hydra-init.environment) HYDRA_DBI;
      };
      path = [ config.services.hydra.package ];
      script = lib.optionalString (cfg.services.hydra.admin.passwordFile != null) ''
        set -e
        export HYDRA_ADMIN_PASSWORD=$(cat ${cfg.services.hydra.admin.passwordFile})
        hydra-create-user ${cfg.services.hydra.admin.user} --role admin --password "$HYDRA_ADMIN_PASSWORD"
      '';
    };
  };
}
