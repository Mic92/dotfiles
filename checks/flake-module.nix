{
  lib,
  self',
  self,
  system,
  ...
}:
let
  machinesPerSystem = {
    aarch64-linux = [
      "blob64"
    ];
    x86_64-linux = [
      "eve"
      "eva"
      "turingmachine"
      "matchbox"
      "bernie"
      "jacquardmachine"
      "dorits-laptop"
    ];
  };
  darwinMachinesPerSystem = {
    aarch64-darwin = [
      "evo"
    ];
  };
  nixosMachines = lib.mapAttrs' (n: lib.nameValuePair "nixos-${n}") (
    lib.genAttrs (machinesPerSystem.${system} or [ ]) (
      name: self.nixosConfigurations.${name}.config.system.build.toplevel
    )
  );
  darwinMachines = lib.mapAttrs' (n: lib.nameValuePair "darwin-${n}") (
    lib.genAttrs (darwinMachinesPerSystem.${system} or [ ]) (
      name: self.darwinConfigurations.${name}.system
    )
  );

  blacklistPackages = [
    "install-iso"
    "nspawn-template"
    "netboot-pixie-core"
    "netboot"
  ];
  packages = lib.mapAttrs' (n: lib.nameValuePair "package-${n}") (
    lib.filterAttrs (n: _v: !(builtins.elem n blacklistPackages)) self'.packages
  );
  devShells = lib.mapAttrs' (n: lib.nameValuePair "devShell-${n}") self'.devShells;
  homeConfigurations = lib.mapAttrs' (
    name: config: lib.nameValuePair "home-manager-${name}" config.activation-script
  ) (self'.legacyPackages.homeConfigurations or { });
in
{
  checks = nixosMachines // darwinMachines // packages // devShells // homeConfigurations;
}
