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

    inventory = {

      services = {
        borgbackup.blob64 = {
          roles.server.machines = [ "blob64" ];
          roles.client.tags = [ "backup" ];
          roles.client.extraModules = [ "machines/modules/borgbackup.nix" ];
        };
        zerotier.mic92 = {
          roles.controller.machines = [ "eve" ];
          roles.moon.machines = [
            "eva"
            "eve"
          ];
          roles.peer.tags = [ "zerotier" ];
        };
      };

      machines = {
        bernie.tags = [
          "backup"
          "zerotier"
        ];
        turingmachine.tags = [
          "backup"
          "zerotier"
        ];
        eve.tags = [
          "backup"
          "zerotier"
        ];
        eva.tags = [
          "backup"
          "zerotier"
        ];
        matchbox.tags = [
          "backup"
          "zerotier"
        ];
      };
    };
  };
}
