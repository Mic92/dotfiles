with import ../krops.nix {
  name = "eddie";
  secretSource = "joerg";
};

pkgs.krops.writeDeploy "deploy" {
  source = lib.evalSource [(defaultSources // {
    nixpkgs.file = nixpkgs.file;
  })];
  target = "root@eddie.r";
  #target = "root@129.215.90.4";
}
