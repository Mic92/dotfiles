{
  nix2container,
  knot-resolver,
  writeText,
  writeScript,
  runtimeShell,
  busybox,
  knot-dns,
  tcpdump,
}: let
  kresd = knot-resolver.override {extraFeatures = true;};
  configFile = writeText "kresd.conf" ''
    net.listen({'0.0.0.0', '::'}, 4443, { kind = 'doh2' })
    net.listen({'0.0.0.0', '::'}, 8853, { kind = 'tls', freebind = true })

    log_level('debug')
    modules.load('predict')

    modules = { 'hints > iterate' }

    net.tls("/tmp/cert.pem", "/tmp/key.pem")

    -- use ns.spaceboys.net as one dns resolver is borked
    policy.add(policy.suffix(policy.STUB('8.8.8.8'), {todname('c3d2.de')}))
    policy.add(policy.suffix(policy.STUB('8.8.8.8'), {todname('spaceboyz.net')}))

    rawset(cache, 'current_storage', 'lmdb:///var/cache/knot-resolver')
  '';
in
  nix2container.buildImage {
    name = "mic92/kresd";
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
          echo "$TLS_CERT" | base64 -d > /tmp/cert.pem
          echo "$TLS_KEY" | base64 -d > /tmp/key.pem
          exec ${kresd}/bin/kresd --noninteractive -c ${configFile}
        '')
      ];
      Volumes = {
        "/var/cache/knot-resolver" = {};
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
