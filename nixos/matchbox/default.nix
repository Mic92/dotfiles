with import ../krops.nix {
  name = "matchbox";
  secretSource = "joerg";
};

pkgs.krops.writeDeploy "deploy" {
  source = lib.evalSource [
    (defaultSources // { nixpkgs.file = nixpkgs.file;})
  ];
  buildTarget = "joerg@localhost";
  crossDeploy = true;
  target = "root@matchbox.r";
}
