{ config, pkgs, ... }:
{
  services.tinc.networks.retiolum = {
    rsaPrivateKeyFile = config.clan.core.vars.generators.retiolum.files."retiolum.rsa_key.priv".path;
    ed25519PrivateKeyFile =
      config.clan.core.vars.generators.retiolum.files."retiolum.ed25519_key.priv".path;
  };

  clan.core.vars.generators.retiolum = {
    files."retiolum.rsa_key.priv" = { };
    files."retiolum.ed25519_key.priv" = { };
    files."retiolum.rsa_key.pub".secret = false;
    files."retiolum.ed25519_key.pub".secret = false;
    runtimeInputs = with pkgs; [
      coreutils
      tinc_pre
    ];
    script = ''
      tinc --config "$out" generate-keys 4096 >/dev/null
      mv "$out"/rsa_key.priv "$out"/retiolum.rsa_key.priv
      mv "$out"/ed25519_key.priv "$out"/retiolum.ed25519_key.priv
      mv "$out"/rsa_key.pub "$out"/retiolum.rsa_key.pub
      mv "$out"/ed25519_key.pub "$out"/retiolum.ed25519_key.pub
    '';
  };

  # only allow connections from hosts specified in our retiolum hosts.
  services.tinc.networks.retiolum.extraConfig = "StrictSubnets yes";
}
