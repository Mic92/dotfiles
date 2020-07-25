with import ../krops.nix;

krops.writeCommand "deploy" {
  source = lib.evalSource [{
    inherit dotfiles;
  }];
  target = "root@eddie.r";
  #target = "root@129.215.90.4";
  command = targetPath: ''
    nixos-rebuild switch --flake ${targetPath}/dotfiles
  '';
}
