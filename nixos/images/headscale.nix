{
  nix2container,
  knot-resolver,
  writeText,
  writeScript,
  runtimeShell,
  busybox,
  headscale,
  tcpdump,
}: let
  kresd = knot-resolver.override {extraFeatures = true;};
  configFile = writeText "headscale.yaml" (builtins.toJSON {
    db_type = "sqlite";
  });
in
  nix2container.buildImage {
    name = "mic92/headscale";
    config = {
      Entrypoint = [
        (writeScript "init" ''
          #!${busybox}/bin/sh
          set -x
          export PATH=${busybox}/bin
          mkdir -p /bin /tmp
          busybox --install -s /bin

          for bin in ${tcpdump}/bin/*; do
            ln -s "$bin" /bin/$(basename $bin)
          done
          mkdir -p /etc/headscale
          ln -s ${configFile} /etc/headscale/config.yaml

          exec ${headscale}/bin/headscale serve
        '')
      ];
      Volumes = {
        "/var/lib/headscale" = {};
      };
    };
    isolatedDeps = [
      (nix2container.buildLayer {
        deps = [
          kresd
          busybox
          tcpdump
        ];
      })
    ];
  }
