{self, ...}: let
  inherit (self.inputs) miniond sops-nix nixpkgs;
  defaultModules = [
    miniond.nixosModule
    ./cloudlab/node.nix
    sops-nix.nixosModules.sops
  ];
  nixosSystem = nixpkgs.lib.makeOverridable nixpkgs.lib.nixosSystem;
in {
  flake.nixosConfigurations = {
    cloudlab-node = nixosSystem {
      system = "x86_64-linux";
      modules = defaultModules;
    };
  };
}
