{
  self,
  config,
  lib,
  ...
}:
{
  srvos.flake = self;
  documentation.info.enable = false;
  clan.core.networking.targetHost = lib.mkDefault "root@${config.networking.hostName}.hyprspace";

  security.sudo.execWheelOnly = lib.mkForce false;
  programs.nano.enable = false;

  imports = [
    ./nix-path.nix
    ./acme.nix
    ./nix-daemon.nix
    ./minimal-docs.nix
    ./nftables.nix
    ./i18n.nix
    ./zfs.nix
    ./thermald.nix
    ./fhs-compat.nix
    ./update-prefetch.nix

    self.inputs.srvos.nixosModules.common
    self.inputs.srvos.nixosModules.mixins-telegraf
    self.inputs.srvos.nixosModules.mixins-nix-experimental
    { networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [ 9273 ]; }
    self.inputs.srvos.nixosModules.mixins-trusted-nix-caches

    ./retiolum.nix
    self.inputs.retiolum.nixosModules.retiolum
    self.inputs.retiolum.nixosModules.ca

    ./zerotier.nix
    self.inputs.nether.nixosModules.hosts
  ];
}
