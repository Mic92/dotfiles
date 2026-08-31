# Shared between eve (creates the lldap mail account) and eva (alertmanager
# authenticates with it against mail.thalheim.io).
{ pkgs, ... }:
{
  clan.core.vars.generators.alertmanager-smtp = {
    share = true;
    files.password = { };
    files.env = { };
    runtimeInputs = [
      pkgs.coreutils
      pkgs.openssl
    ];
    script = ''
      openssl rand -hex 24 > "$out/password"
      printf 'SMTP_PASSWORD=%s' "$(cat "$out/password")" > "$out/env"
    '';
  };
}
