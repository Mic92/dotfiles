with import ../krops.nix {
  name = "eddie";
  secretSource = "joerg";
};

pkgs.krops.writeDeploy "deploy" {
  source = lib.evalSource [{
    nixpkgs.file = nixpkgs.file;
    inherit dotfiles nixos-config secrets shared-secrets;
  }];
  target = "root@eddie.r";
  #target = "root@129.215.90.4";
}
