{ self, ... }:
{
  flake.nixosModules.default = ../nixosModules/default.nix;
  clan = {
    meta.name = "mic92";

    pkgsForSystem = system: self.inputs.nixpkgs.legacyPackages.${system};

    inventory = {
      tags =
        { config, ... }:
        {
          backup = builtins.filter (name: name != "blob64") config.nixos;
        };

      machines.evo.machineClass = "darwin";

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
          roles.peer.tags = [ "nixos" ];
        };

        sshd.mic92 = {
          roles.server.tags = [ "nixos" ];
          roles.client.tags = [ "nixos" ];
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
