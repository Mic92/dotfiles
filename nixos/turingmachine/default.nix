with import ../krops.nix;

krops.writeCommand "deploy" {
  source = lib.evalSource [{
    inherit dotfiles;
  }];
  target = lib.mkTarget "joerg@turinmachine.r" // {
    sudo = true;
  };
  command = targetPath: ''
    nixos-rebuild switch --flake ${targetPath}/dotfiles
  '';
}
