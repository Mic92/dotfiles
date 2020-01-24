with (import <nixpkgs> {}).callPackage ../krops.nix {
  name = "matchbox";
  secretSource = "joerg";
};

pkgs.krops.writeDeploy "deploy" {
  source = lib.evalSource [{
    nixpkgs.file = nixpkgs.file;
    inherit dotfiles nixos-config secrets shared-secrets;
  }];
  buildTarget = "joerg@localhost";
  crossDeploy = true;
  target = "root@matchbox.r";
}
