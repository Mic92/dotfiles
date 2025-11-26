{ self, lib, ... }:
{
  flake.nixosModules.default = ../nixosModules/default.nix;
  clan = {
    meta.name = "mic92";

    pkgsForSystem = system: self.inputs.nixpkgs.legacyPackages.${system};

    inventory = {
      tags =
        { config, ... }:
        {
          backup = builtins.filter (name: name != "blob64" && name != "installer") config.nixos;
          wireguard-peers = builtins.filter (name: name != "eve" && name != "eva") config.nixos;
        };

      machines.evo.machineClass = "darwin";

      instances = {
        emergency-access = {
          module.name = "emergency-access";
          module.input = "clan-core";
          roles.default.tags.nixos = { };
        };

        borgbackup-blob64 = {
          module.name = "borgbackup";
          module.input = "clan-core";
          roles.server.machines.blob64 = { };
          roles.server.settings = {
            directory = "/zdata/borg";
          };
          roles.client.tags.backup = { };
          roles.client.extraModules = [ ../nixosModules/borgbackup.nix ];
        };

        zerotier-mic92 = {
          module.name = "zerotier";
          module.input = "clan-core";
          roles.controller.machines.eve = { };
          roles.moon.machines.eva.settings = {
            stableEndpoints = [
              "116.203.179.132"
              "2a01:4f8:1c1a:37b2::1"
            ];
          };
          roles.moon.machines.eve.settings = {
            stableEndpoints = [
              "95.217.199.121"
              "2a01:4f9:4a:42e8::1"
            ];
          };
          roles.peer.tags.nixos = { };
        };

        sshd-mic92 = {
          module.name = "sshd";
          module.input = "clan-core";
          roles.server.tags.nixos = { };
          roles.client.tags.nixos = { };
          # tor-hidden-service
          roles.client.extraModules = [ ../nixosModules/ssh.nix ];
          roles.client.settings = {
            certificate.searchDomains = [
              "i"
              "r"
              "hyprspace"
              "local"
              "onion"
              "thalheim.io"
            ];
          };
        };

        wireguard = {
          module.name = "wireguard";
          module.input = "clan-core";

          roles.controller.settings.domain = "x";
          roles.peer.settings.domain = "x";

          roles.controller.machines.eva = {
            settings = {
              endpoint = "eva.i";
              port = 51820;
            };
          };
          roles.controller.machines.eve = {
            settings = {
              endpoint = "eve.i";
              port = 51821;
            };
          };

          roles.peer.settings.controller = "eva";
          roles.peer.tags.wireguard-peers = { };

          roles.peer.machines.turingmachine.settings.controller = lib.mkForce "eve";
          roles.peer.machines.evo.settings.controller = "eva";
        };

        internet = {
          module.name = "internet";
          module.input = "clan-core";
          roles.default.machines.eve = {
            settings.host = "eve.i";
          };
          roles.default.machines.eva = {
            settings.host = "eva.i";
          };
        };

        tor.roles.server.tags.nixos = { };

        users-root = {
          module.name = "users";
          module.input = "clan-core";
          roles.default.tags.nixos = { };
          roles.default.settings = {
            user = "root";
            prompt = false;
            groups = [ ];
          };
        };

        localbackup-turingmachine = {
          module.name = "localbackup";
          module.input = "clan-core";
          roles.default.machines.turingmachine.settings = {
            targets.hdd = {
              directory = "/backup-2tb/turingmachine";
              mountpoint = "/backup-2tb";
              preMountHook = "localbackup-unlock-hdd";
            };
          };
          roles.default.extraModules = [ ../machines/turingmachine/modules/localbackup.nix ];
        };
      };
    };
  };
}
