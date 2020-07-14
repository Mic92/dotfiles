with import <nixpkgs> {};

let
  outputs = builtins.getFlake (toString ../.);
  drvs = lib.collect lib.isDerivation outputs.hydraJobs;
in drvs

