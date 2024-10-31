{ self, inputs, ... }:
{
  flake.nixosModules.default = ./modules/default.nix;
  clan = {
    meta.name = "mic92";
    directory = self;
    specialArgs = {
      self = self;
      inputs = inputs;
    };
    pkgsForSystem = system: inputs.nixpkgs.legacyPackages.${system};

    inventory.services = {
      borgbackup.blob64 = {
        roles.server.machines = [ "blob64" ];
        roles.client.tags = [ "backup" ];
        roles.client.extraModules = [ "machines/modules/borgbackup.nix" ];
      };
    };

    inventory.machines = {
      bernie.tags = [ "backup" ];
      turingmachine.tags = [ "backup" ];
      eve.tags = [ "backup" ];
      eva.tags = [ "backup" ];
      matchbox.tags = [ "backup" ];
    };
  };
}
