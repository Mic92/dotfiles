{ self, inputs, ... }:
{
  flake.nixosModules.default = ../nixosModules/default.nix;
  clan = {
    meta.name = "mic92";

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
          roles.server.config = {
            directory = "/zdata/borg";
          };
          roles.client.tags = [ "backup" ];
          roles.client.extraModules = [ "nixosModules/borgbackup.nix" ];
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
          # tor-hidden-service
          roles.client.extraModules = [ "nixosModules/ssh.nix" ];

          config.certificate.searchDomains = [
            "i"
            "r"
            "hyprspace"
            "local"
            "onion"
            "thalheim.io"
          ];
        };
      };
    };
  };
}
