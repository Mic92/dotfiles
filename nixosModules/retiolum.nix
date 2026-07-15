{
  config,
  pkgs,
  ...
}:
{
  networking.retiolum.ed25519PrivateKeyFile =
    config.clan.core.vars.generators.retiolum.files."retiolum.ed25519_key.priv".path;

  services.tincr.networks.retiolum.extraConfig = ''
    StrictSubnets = yes
    DhtDiscovery = yes
    UPnP = yes
  '';

  clan.core.vars.generators.retiolum = {
    files."retiolum.ed25519_key.priv" = { };
    files."retiolum.ed25519_key.pub".secret = false;
    runtimeInputs = with pkgs; [
      coreutils
      tinc_pre
    ];
    script = ''
      tinc --batch --config "$out" generate-ed25519-keys
      mv "$out"/ed25519_key.priv "$out"/retiolum.ed25519_key.priv
      mv "$out"/ed25519_key.pub  "$out"/retiolum.ed25519_key.pub
    '';
  };
}
