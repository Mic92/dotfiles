{ self, ... }:
{
  nix.nixPath = [ "nixpkgs=${self.inputs.nixpkgs}" ];
}
