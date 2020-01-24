{ fetchFromGitHub, name, secretSource ? "shared" }: rec {
  krops = fetchFromGitHub {
    owner = "krebs";
    repo = "krops";
    rev = "v1.19.0";
    sha256 = "0fcybc92nslwvzw7vz5b5qwpxbjppz4kckbvbrxrr536kd5ynwzw";
  };
  lib = import "${krops}/lib";
  pkgs = import "${krops}/pkgs" {};

  krops-local = /home/joerg/git/krops;

  nixpkgs.git = {
    clean.exclude = ["/.version-suffix"];
    ref = "34a39207175e81564b85cc65c2ecaed1fd6f8c66";
    url = https://github.com/Mic92/nixpkgs;
  };

  nixos-hardware.git = {
    clean.exclude = ["/.version-suffix"];
    ref = "89c4ddb0e60e5a643ab15f68b2f4ded43134f492";
    url = https://github.com/NixOS/nixos-hardware;
  };

  nixpkgs.file = {
    exclude = [".git"];
    path = toString <nixpkgs>;
  };

  nixos-config.symlink = "dotfiles/nixos/${name}/configuration.nix";

  secrets.pass = {
    dir  = toString ./. + "/secrets/${secretSource}";
    name = name;
  };

  shared-secrets.pass = {
    dir  = toString ./secrets/shared;
    name = "shared";
  };

  dotfiles.file = {
    path = toString ./..;
    exclude = [ ".git" ];
  };
}
