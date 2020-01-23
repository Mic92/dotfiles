let
  krops = (import <nixpkgs> {}).callPackage ../krops.nix {};
  lib = import "${krops}/lib";
  pkgs = import "${krops}/pkgs" {};

  source = lib.evalSource [{
    #nixpkgs.file = {
    #  path = toString <nixpkgs>;
    #  exclude = [".git"];
    #};
    nixpkgs.git = import ../nixpkgs.nix;
    dotfiles.file.path = toString ./../..;
    nixos-config.symlink = "dotfiles/nixos/eve/configuration.nix";

    secrets.pass = {
      dir  = toString ../secrets/shared;
      name = "eve";
    };

    shared-secrets.pass = {
      dir  = toString ../secrets/shared;
      name = "shared";
    };
  }];
in pkgs.krops.writeDeploy "deploy" {
  source = source;
  target = "root@eve.thalheim.io";
}
