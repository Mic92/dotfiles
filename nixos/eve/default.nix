with import ../krops.nix {
  name = "eve";
};

pkgs.krops.writeDeploy "deploy" {
  source = lib.evalSource [defaultSources];
  target = "root@eve.thalheim.io";
  #target = "root@129.215.90.4";
}
