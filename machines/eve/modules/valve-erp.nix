# Valve ERP — medical device traceability system.
{
  self,
  config,
  pkgs,
  ...
}:
{
  imports = [ self.inputs.valve-erp.nixosModules.default ];

  services.valve-erp = {
    enable = true;
    package = self.inputs.valve-erp.packages.${pkgs.stdenv.hostPlatform.system}.valve-erp;
    domain = "valve.thalheim.io";
    secretKeyFile = config.clan.core.vars.generators.valve-erp.files.secret-key.path;
    adminPasswordFile = config.clan.core.vars.generators.valve-erp.files.admin-password.path;
    smtp = {
      host = "localhost";
      from = "valve@thalheim.io";
    };
  };

  clan.core.vars.generators.valve-erp = {
    files.secret-key = { };
    files.admin-password = { };
    runtimeInputs = with pkgs; [
      openssl
      xkcdpass
    ];
    script = ''
      openssl rand -hex 50 | tr -d '\n' > $out/secret-key
      xkcdpass -n 4 -d - | tr -d '\n' > $out/admin-password
    '';
  };
}
