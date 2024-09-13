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
