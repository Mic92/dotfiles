{
  config,
  lib,
  pkgs,
  self,
  ...
}:
{
  services.tinc.networks.retiolum = {
    # Rust rewrite, drop-in for tinc_pre on the NixOS module's argv.
    package = self.inputs.tincr.packages.${pkgs.stdenv.hostPlatform.system}.tincd;
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
  services.tinc.networks.retiolum.extraConfig = ''
    StrictSubnets yes
    DhtDiscovery yes
    DhtSecretFile = /var/lib/tinc/retiolum/dht_secret
    UPnP yes
  '';

  # /etc/tinc/retiolum is read-only on NixOS (built from the store);
  # the Rust tincd's address cache falls back to $STATE_DIRECTORY/cache
  # when confbase isn't writable. Upstream nixpkgs tinc module sets no
  # StateDirectory, so the fallback had nowhere to go and every outgoing
  # retry logged EACCES. tincd starts as root and drops to tinc-retiolum
  # via `-U` (no `User=`), so systemd creates StateDirectory owned by
  # root — chown it in preStart (which already runs as root in the
  # upstream module) before the daemon drops privileges.
  systemd.services."tinc.retiolum" = {
    serviceConfig.StateDirectory = "tinc/retiolum";
    preStart = lib.mkAfter ''
      install -d -m 0750 -o tinc-retiolum -g tinc-retiolum /var/lib/tinc/retiolum/cache
      # DhtSecretFile is required when DhtDiscovery=yes parses it; the
      # file is provisioned out-of-band (clan vars / manual scp). Fail
      # early with a clear message instead of tincd's generic config error.
      test -s /var/lib/tinc/retiolum/dht_secret || {
        echo "retiolum: /var/lib/tinc/retiolum/dht_secret missing (32B raw or b64)" >&2
        exit 1
      }
    '';
  };
}
