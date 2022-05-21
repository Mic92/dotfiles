{
  description = "Description for the project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-modules-core.url = "github:hercules-ci/flake-modules-core";
    flake-modules-core.inputs.nixpkgs.follows = "nixpkgs";
    poetry2nix.url = "github:nix-community/poetry2nix";
    poetry2nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    flake-modules-core,
    ...
  }:
    flake-modules-core.lib.mkFlake {inherit self;} {
      systems = ["x86_64-linux"];
      perSystem = system: {
        config,
        self',
        inputs',
        pkgs,
        ...
      }: let
        poetry2nix = pkgs.callPackage self.inputs.poetry2nix {};
      in {
        devShells.default = pkgs.mkShell {
          buildInputs =
            (poetry2nix.mkPoetryPackages {
              projectDir = ./.;
            })
            .poetryPackages
            ++ [pkgs.python3.pkgs.poetry];
        };
      };
    };
}
