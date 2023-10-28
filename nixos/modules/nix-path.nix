{ inputs, ... }: {
  nix.nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
}
