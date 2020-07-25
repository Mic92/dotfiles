with import ../krops.nix;

krops.writeCommand "deploy" {
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
