{ self, inputs, ... }:
{
  flake.nixosModules.default =
    { config, lib, ... }:
    {
      srvos.flake = self;
      documentation.info.enable = false;
      services.envfs.enable = true;
      clan.core.networking.targetHost = lib.mkDefault "root@${config.networking.hostName}.r";

      imports = [
        ./modules/nix-path.nix
        ./modules/pinned-registry.nix
        ./modules/acme.nix
        ./modules/nix-daemon.nix
        ./modules/minimal-docs.nix
        ./modules/i18n.nix
        ./modules/sshd
        ./modules/zfs.nix
        ./modules/thermald.nix

        inputs.clan-core.clanModules.sshd
        inputs.clan-core.clanModules.borgbackup
        inputs.clan-core.clanModules.zerotier

        inputs.srvos.nixosModules.common
        inputs.srvos.nixosModules.mixins-telegraf
        inputs.srvos.nixosModules.mixins-nix-experimental
        { networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [ 9273 ]; }
        inputs.srvos.nixosModules.mixins-trusted-nix-caches

        ./modules/retiolum.nix
        inputs.retiolum.nixosModules.retiolum
        inputs.retiolum.nixosModules.ca

        ./modules/zerotier.nix
        inputs.nether.nixosModules.hosts
      ];
    };
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
        roles.client.imports = [ "machines/modules/borgbackup.nix" ];
      };
      zerotier.mic92 = {
        roles.peer.tags = [ "all" ];
        roles.peer.imports = [ "machines/modules/zerotier.nix" ];
        roles.moon.machines = [ "eve" "eva" ];
        #roles.moon.perMachineConfig = {
        #  eve = {
        #    moon.stableEndpoints = [ "95.217.199.121" "2a01:4f9:4a:42e8::1" ];
        #  };
        #  eva = {
        #    moon.stableEndpoints = [ "89.58.27.144" "2a03:4000:62:fdb::" ];
        #  };
        #};
        #roles.moon.machinesAttr = {
        #  eve.config = {};
        #  eva.config = {};
        #};
        #roles.moon.machines = [ "eve" "eva" ];

        roles.moon.config = {};

        machines.eve.config = {
          moon.stableEndpoints = [ "95.217.199.121" "2a01:4f9:4a:42e8::1" ];
        };
        machines.eva.config = {
          moon.stableEndpoints = [ "89.58.27.144" "2a03:4000:62:fdb::" ];
        };

        roles.controller.machines = [ "eve" ];
      };
    };
    inventory.machines = {
      bernie.tags = [ "all" "backup" ];
      turingmachine.tags = [ "all" "backup" ];
      eve.tags = [ "all" "backup" ];
      eva.tags = [ "all" "backup" ];
      matchbox.tags = [ "all" "backup" ];
    };
  };
}
