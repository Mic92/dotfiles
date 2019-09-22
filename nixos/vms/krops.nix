let
  krops = (import <nixpkgs> {}).fetchgit {
    url = https://cgit.krebsco.de/krops/;
    rev = "ad4c3cad0a04d1e733b3d3007d4a1236b0b66353";
    sha256 = "0v07hpjywjfbyfg7zz0kdd2n4wwz7f1qs4mccqsa5zvrbhsd9dvq";
  };

  lib = import "${krops}/lib";
  pkgs = import "${krops}/pkgs" {};

  source = lib.evalSource [{
    nixpkgs.git = {
      clean.exclude = ["/.version-suffix"];
      ref = "28ce25204a349892878e489328498db35517a9be";
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
