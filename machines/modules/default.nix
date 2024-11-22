{
  inputs,
  config,
  lib,
  ...
}:
{
  srvos.flake = inputs.self;
  documentation.info.enable = false;
  clan.core.networking.targetHost = lib.mkDefault "root@${config.networking.hostName}.r";

  security.sudo.execWheelOnly = lib.mkForce false;

  imports = [
    ./nix-path.nix
    ./acme.nix
    ./nix-daemon.nix
    ./minimal-docs.nix
    ./i18n.nix
    ./zfs.nix
    ./thermald.nix
    ./fhs-compat.nix
    ./update-prefetch.nix

    inputs.srvos.nixosModules.common
    inputs.srvos.nixosModules.mixins-telegraf
    inputs.srvos.nixosModules.mixins-nix-experimental
    { networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [ 9273 ]; }
    inputs.srvos.nixosModules.mixins-trusted-nix-caches

    ./retiolum.nix
    inputs.retiolum.nixosModules.retiolum
    inputs.retiolum.nixosModules.ca

    ./zerotier.nix
    inputs.nether.nixosModules.hosts
  ];
}
