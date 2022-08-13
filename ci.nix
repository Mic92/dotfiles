{ self, ... }:
let
  lib = self.inputs.nixpkgs.lib;
in {
  flake.hydraJobs =
    (lib.mapAttrs' (name: config: lib.nameValuePair "nixos-${name}" config.config.system.build.toplevel) self.nixosConfigurations)
    // (lib.mapAttrs' (name: config: lib.nameValuePair "home-manager-${name}" config.activation-script) self.hmConfigurations)
    // {
      alwaysFails = self.inputs.nixpkgs.legacyPackages.x86_64-linux.runCommand "foo" {} ''
        false
      '';
    };
}
