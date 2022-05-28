{
  description = "Description for the project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    poetry2nix.url = "github:nix-community/poetry2nix";
    poetry2nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit self;} {
      systems = ["x86_64-linux"];
      perSystem = {
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
