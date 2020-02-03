with import ../krops.nix {
  name = "turingmachine";
  secretSource = "joerg";
};

pkgs.krops.writeDeploy "deploy" {
  source = lib.evalSource [{
    nixpkgs.file = nixpkgs.file;
    inherit dotfiles nixos-config secrets shared-secrets nixos-hardware nur;
  }];
  target = lib.mkTarget "joerg@localhost" // {
    sudo = true;
  };
}
