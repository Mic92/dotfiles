with import ../krops.nix {
  name = "turingmachine";
  secretSource = "joerg";
};

pkgs.krops.writeDeploy "deploy" {
  source = lib.evalSource [(defaultSources // {
    nixpkgs.file = nixpkgs.file;
  })];
  target = lib.mkTarget "joerg@localhost" // {
    sudo = true;
  };
}
