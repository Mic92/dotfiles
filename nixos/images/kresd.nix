{ nix2container
, knot-resolver
, writeText
}:

let
  kresd = knot-resolver.override { extraFeatures = true; };
  configFile = writeText "kresd.conf" ''
    net.listen({'0.0.0.0', '::'}, 53, { kind = 'dns' })
    log_level('debug')
    modules.load('predict')

    modules = { 'hints > iterate' }

    -- use ns.spaceboys.net as one dns resolver is borked
    policy.add(policy.suffix(policy.STUB('8.8.8.8'), {todname('c3d2.de')}))
    policy.add(policy.suffix(policy.STUB('8.8.8.8'), {todname('spaceboyz.net')}))

    rawset(cache, 'current_storage', 'lmdb:///var/cache/knot-resolver')
  '';
in nix2container.buildImage {
  name = "mic92/kresd";
  config = {
    Entrypoint = [ "${kresd}/bin/kresd" "--noninteractive" "-c" configFile ];
    Volumes = {
      "/var/cache/knot-resolver" = {};
    };
  };
}
