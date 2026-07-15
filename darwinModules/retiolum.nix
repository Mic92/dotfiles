{
  config,
  pkgs,
  ...
}:
{
  networking.retiolum.ed25519PrivateKeyFile = "/etc/tinc/retiolum/ed25519_key.priv";

  services.tincr.networks.retiolum.extraConfig = ''
    StrictSubnets = yes
    DhtDiscovery = yes
    UPnP = yes
  '';

  environment.systemPackages = [
    (pkgs.writeShellScriptBin "tinc.retiolum" ''
      exec ${config.services.tincr.networks.retiolum.package}/bin/tinc \
        --pidfile=/var/run/tincr-retiolum.pid \
        --config=/etc/tinc/retiolum "$@"
    '')
  ];
}
