{ self, inputs, ... }:
{
  flake.darwinConfigurations.evo = inputs.nix-darwin.lib.darwinSystem {
    modules = [ ./evo/configuration.nix ];
    specialArgs = {
      inherit self;
      inherit inputs;
    };
  };
}
