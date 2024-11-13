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
      tags =
        { config, ... }:
        {
          backup = builtins.filter (name: name != "blob64") config.all;
        };
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
          roles.peer.tags = [ "all" ];
        };

        sshd.mic92 = {
          roles.server.tags = [ "all" ];
          roles.client.tags = [ "all" ];
          roles.client.extraModules = [ "machines/modules/ssh.nix" ];

          config.certificate.searchDomains = [
            "i"
            "r"
            "thalheim.io"
          ];
        };
      };
    };
  };
}
