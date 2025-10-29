{ config, pkgs, ... }:
{
  imports = [ ../../../nixosModules/phpldapadmin ];

  services.phpldapadmin = {
    enable = true;
    domain = "ldap.thalheim.io";

    # Generated via clan.core.vars.generators
    appKey = config.clan.core.vars.generators.phpldapadmin.files.app-key.path;

    # Use local PostgreSQL database
    database.createLocally = true;

    nginx.enable = true;

    poolSettings = {
      "pm" = "dynamic";
      "pm.max_children" = 16;
      "pm.start_servers" = 2;
      "pm.min_spare_servers" = 2;
      "pm.max_spare_servers" = 4;
      "pm.max_requests" = 500;
    };
  };

  # Add SSL configuration
  services.nginx.virtualHosts."ldap.thalheim.io" = {
    forceSSL = true;
    enableACME = true;
  };

  # Generate Laravel app key using clan vars generator
  # Format: base64:44-character-string (32 bytes)
  clan.core.vars.generators.phpldapadmin = {
    files.app-key = { };
    migrateFact = "phpldapadmin";
    runtimeInputs = [
      pkgs.openssl
      pkgs.coreutils
    ];
    script = ''
      # Generate Laravel app key in the correct format (base64:32-bytes)
      # Note: tr strips the trailing newline from openssl output
      printf "base64:%s" "$(openssl rand -base64 32 | tr -d '\n')" > "$out"/app-key
    '';
  };
}
