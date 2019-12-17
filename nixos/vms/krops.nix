let
  krops = (import <nixpkgs> {}).fetchgit {
    url = https://cgit.krebsco.de/krops/;
    rev = "refs/tags/v1.17.0";
    sha256 = "150jlz0hlb3ngf9a1c9xgcwzz1zz8v2lfgnzw08l3ajlaaai8smd";
  };

  lib = import "${krops}/lib";
  pkgs = import "${krops}/pkgs" {};

  source = lib.evalSource [{
    nixpkgs.git = {
      clean.exclude = ["/.version-suffix"];
      ref = "14110e71b0d4842ae4de44331c12e2e57dc6e44b";
      url = https://github.com/Mic92/nixpkgs;
    };
    dotfiles.file.path = toString ./../..;
    nixos-config.symlink = "dotfiles/nixos/vms/eve/configuration.nix";

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
  target = "root@eve.r";
}
