let
  flake = builtins.getFlake (toString ../.);
  pkgs = import flake.inputs.nixpkgs {};
  lib = import "${flake.inputs.krops}/lib";
  krops = pkgs.callPackage "${flake.inputs.krops}/pkgs/krops" {
    populate = pkgs.callPackage "${flake.inputs.krops}/pkgs/populate" {};
  };
in {
  inherit lib krops;

  dotfiles.file = {
    path = toString ./..;
    filters = [{
      type = "exclude";
      pattern = ".git";
    }];
  };
}
