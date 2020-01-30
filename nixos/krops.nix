{ name, secretSource ? "shared" }:
let
  # use `niv` for updating this
  sources = import ./nix/sources.nix;
  sourcesJson = builtins.fromJSON (builtins.readFile ./nix/sources.json);
in rec {
  inherit (sources) krops;
  lib = import "${krops}/lib";
  pkgs = import "${krops}/pkgs" {};

  krops-local = /home/joerg/git/krops;

  nixpkgs.git = {
    clean.exclude = ["/.version-suffix"];
    url = https://github.com/Mic92/nixpkgs;
    ref = sourcesJson.nixpkgs.rev;
  };

  nixos-hardware.git = {
    clean.exclude = ["/.version-suffix"];
    url = https://github.com/NixOS/nixos-hardware;
    ref = sourcesJson.nixos-hardware.rev;
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
