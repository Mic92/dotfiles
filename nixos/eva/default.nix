with import ../krops.nix;

krops.writeCommand "deploy" {
  source = lib.evalSource [{
    inherit dotfiles;
  }];
  # 1GB goes OOM while deploying
  target = "root@eve";
  command = targetPath: ''
    nix shell 'nixpkgs#git' -c \
      nixos-rebuild switch --flake ${targetPath}/dotfiles#eva \
      --build-host localhost \
      --target-host root@eva.thalheim.io
  '';
}
