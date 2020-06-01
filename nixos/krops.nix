{ name, secretSource ? "shared" }:
let
  # use `niv` for updating this
  sources = import ./nix/sources.nix;
  sourcesJson = builtins.fromJSON (builtins.readFile ./nix/sources.json);
in rec {
  inherit (sources) krops;
  lib = import "${krops}/lib";
  pkgs = import "${krops}/pkgs" {};

  defaultSources = {
    nixpkgs.git = nixpkgs.git;
    inherit dotfiles nixos-config secrets shared-secrets nixos-hardware nur home-manager;
  };

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
    filters = [{
      type = "exclude";
      pattern = ".git";
    }];
    path = toString <nixpkgs>;
  };

  nixos-config.symlink = "dotfiles/nixos/${name}/configuration.nix";

  nur.git = {
    url = https://github.com/nix-community/nur;
    ref = sourcesJson.nur.rev;
  };

  home-manager.git = {
    url = https://github.com/rycee/home-manager;
    ref = sourcesJson.home-manager.rev;
  };

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
    filters = [{
      type = "exclude";
      pattern = ".git";
    }];
  };
}
