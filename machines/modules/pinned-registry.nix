{ inputs, ... }:
{
  nix.registry = {
    nixpkgs.to = {
      type = "path";
      path = inputs.nixpkgs;
    };
    home-manager.to = {
      type = "path";
      path = inputs.home-manager;
    };
  };
}
