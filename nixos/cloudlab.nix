{ self, ... }:
let
  inherit (self.inputs) miniond sops-nix nixpkgs;
  defaultModules = [
    miniond.nixosModule
    ./cloudlab/node.nix
    sops-nix.nixosModules.sops
  ];
  nixosSystem = nixpkgs.lib.makeOverridable nixpkgs.lib.nixosSystem;
in
{
  flake.nixosConfigurations = {
    cloudlab-node = nixosSystem {
      system = "x86_64-linux";
      modules = defaultModules;
    };

    cloudlab-k3s-server = nixosSystem {
      system = "x86_64-linux";
      modules =
        defaultModules
        ++ [
          ./modules/k3s/server.nix
        ];
    };

    cloudlab-k3s-agent = nixosSystem {
      system = "x86_64-linux";
      modules =
        defaultModules
        ++ [
          ./modules/k3s/agent.nix
        ];
    };
  };
}
