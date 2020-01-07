let
  krops = (import <nixpkgs> {}).callPackage ./krops.nix {};
  lib = import "${krops}/lib";
  pkgs = import "${krops}/pkgs" {};

  source = lib.evalSource [{
    #nixpkgs.git = import ./nixpkgs.nix;
    nixpkgs.file = {
      path = toString <nixpkgs>;
      exclude = [ ".git" ];
    };
    dotfiles.file.path = toString ./../..;
    nixos-config.symlink = "dotfiles/nixos/vms/eddie/configuration.nix";

    secrets.pass = {
      dir  = toString ../secrets/joerg;
      name = "eddie";
    };

    shared-secrets.pass = {
      dir  = toString ../secrets/shared;
      name = "shared";
    };
  }];
in pkgs.krops.writeDeploy "deploy" {
  source = source;
  target = "root@eddie.r";
  #target = "root@129.215.90.4";
}
