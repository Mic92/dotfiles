with import ../krops.nix {
  name = "eve";
};

pkgs.krops.writeCommand "deploy" {
  source = lib.evalSource [{
    inherit dotfiles;
  }];
  target = "root@eve.thalheim.io";
  #target = "root@129.215.90.4";
  #target = "root@eve.i";
  command = targetPath: ''
    nixos-rebuild switch --flake ${targetPath}/dotfiles
  '';
}
