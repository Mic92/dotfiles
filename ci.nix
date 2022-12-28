{ self, ... }:
let
  inherit (self.inputs.nixpkgs) lib;
in
{
  flake.hydraJobs =
    (lib.mapAttrs' (name: config: lib.nameValuePair "nixos-${name}" config.config.system.build.toplevel) self.nixosConfigurations)
    // (lib.mapAttrs' (name: config: lib.nameValuePair "home-manager-${name}" config.activation-script) self.homeConfigurations);
}
