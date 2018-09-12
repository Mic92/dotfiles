(import <nixpkgs/nixos/lib/eval-config.nix> {
 system = builtins.currentSystem;
 modules = [ <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
             ./myconfig.nix ];
}).config.system.build.isoImage
