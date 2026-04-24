{
  pkgs,
  self,
  ...
}:
{
  # Swap the C tinc_pre daemon for the Rust rewrite on Darwin as well.
  # The retiolum darwin module defaults `package` to pkgs.tinc_pre; tincr
  # ships a drop-in `tincd` (and a `tinc` helper) with the same argv, so
  # the upstream launchd script keeps working unchanged.
  services.tinc.networks.retiolum = {
    package = self.inputs.tincr.packages.${pkgs.stdenv.hostPlatform.system}.tincd;

    extraConfig = ''
      StrictSubnets yes
      DhtDiscovery yes
      UPnP yes
    '';
  };

  # The retiolum darwin module only emits a `tinc.<net>` cli wrapper when
  # the package version is >= 1.1pre (it keys off the tinc_pre versioning
  # scheme). tincr reports its own crate version (0.x), so the wrapper is
  # skipped — provide it ourselves so `tinc.retiolum ...` keeps working.
  environment.systemPackages = [
    (pkgs.writeShellScriptBin "tinc.retiolum" ''
      exec ${self.inputs.tincr.packages.${pkgs.stdenv.hostPlatform.system}.tincd}/bin/tinc \
        --pidfile=/var/run/tinc.retiolum.pid \
        --config=/etc/tinc/retiolum "$@"
    '')
  ];
}
