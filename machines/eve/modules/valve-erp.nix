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
    smtp = {
      host = "localhost";
      from = "valve@thalheim.io";
    };
  };

  clan.core.vars.generators.valve-erp = {
    files.secret-key = { };
    runtimeInputs = [ pkgs.openssl ];
    script = ''
      openssl rand -hex 50 | tr -d '\n' > $secrets/secret-key
    '';
  };
}
