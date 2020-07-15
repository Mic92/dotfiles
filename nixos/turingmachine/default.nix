with import ../krops.nix {
  name = "turingmachine";
  secretSource = "joerg";
};

pkgs.krops.writeCommand "deploy" {
  source = lib.evalSource [{
    inherit dotfiles;
  }];
  target = lib.mkTarget "joerg@localhost" // {
    sudo = true;
  };
  command = targetPath: ''
    nixos-rebuild switch --flake ${targetPath}/dotfiles
  '';
}
