let
  krops = (import <nixpkgs> {}).callPackage ./krops.nix {};
  lib = import "${krops}/lib";
  pkgs = import "${krops}/pkgs" {};

  source = lib.evalSource [{
    nixpkgs.file =  {
      path = toString <nixpkgs>;
      exclude = [ ".git" ];
    };
    nixos-hardware.git = {
      clean.exclude = ["/.version-suffix"];
      ref = "89c4ddb0e60e5a643ab15f68b2f4ded43134f492";
      url = https://github.com/NixOS/nixos-hardware;
    };
    dotfiles.file = {
      path = toString ./../..;
      exclude = [ ".git" ];
    };
    nixos-config.symlink = "dotfiles/nixos/vms/turingmachine/configuration.nix";
    secrets.pass = {
      dir  = toString ../secrets/joerg;
      name = "turingmachine";
    };
    shared-secrets.pass = {
      dir  = toString ../secrets/shared;
      name = "shared";
    };
  }];
in pkgs.krops.writeDeploy "deploy" {
  source = source;
  target = lib.mkTarget "joerg@localhost" // {
    sudo = true;
  };
}
