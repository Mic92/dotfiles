{
  nixpkgs.config.packageOverrides = pkgs: {
    nur = import <nur> { inherit pkgs; };
  };
}
