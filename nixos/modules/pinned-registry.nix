{ inputs, ... }:
{
  nix.registry = {
    nixpkgs.to = {
      type = "path";
      path = inputs.nixpkgs;
    };
    home-manager.to = {
      type = "path";
      path = inputs.nixpkgs;
    };
    nur.to = {
      type = "path";
      path = inputs.nur;
    };
  };
}
