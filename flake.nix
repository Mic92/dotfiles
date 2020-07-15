{
  description = "NixOS configuration with flakes";

  # To update all inputs:
  # $ nix flake update --recreate-lock-file
  inputs.nixpkgs.url = github:Mic92/nixpkgs/master;
  inputs.nur.url = github:nix-community/NUR;
  inputs.sops-nix.url = github:Mic92/sops-nix;
  # for development
  #inputs.sops-nix.url = "/home/joerg/git/sops-nix";
  inputs.sops-nix.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nixos-hardware.url = github:Mic92/nixos-hardware/master;
  inputs.home-manager.url = github:rycee/home-manager;
  inputs.home-manager.flake = false;

  outputs = { self, nixpkgs, nixos-hardware, sops-nix, nur, home-manager }:
    let
      legacyNixPath = {
        nix.nixPath = [
          "home-manager=${home-manager}"
          "nixpkgs=${nixpkgs}"
          "nur=${nur}"
        ];
      };
    in {
      nixosConfigurations.turingmachine = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          { nixpkgs.overlays = [ nur.overlay ]; }
          nixos-hardware.nixosModules.dell-xps-13-9380
          sops-nix.nixosModules.sops
          ./nixos/turingmachine/configuration.nix
          legacyNixPath
        ];
      };
      nixosConfigurations.eddie = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          { nixpkgs.overlays = [ nur.overlay ]; }
          sops-nix.nixosModules.sops
          ./nixos/eddie/configuration.nix
          legacyNixPath
        ];
      };

      hmConfigurations = let
        pkgs = nixpkgs.legacyPackages."x86_64-linux";
        extendedLib = import "${home-manager}/modules/lib/stdlib-extended.nix" pkgs.lib;
        buildHomeManager = configuration: rec {
          hmModules = import "${home-manager}/modules/modules.nix" {
            inherit pkgs;
            lib = extendedLib;
            useNixpkgsModule = true;
          };
          module = extendedLib.evalModules {
            modules = [
              {
                nixpkgs.config.packageOverrides = pkgs: {
                  nur = import "${nur}" {
                    inherit pkgs;
                  };
                };
              }
              configuration
            ] ++ hmModules;
          };
          activate = pkgs.writeShellScript "home-manager-activate" ''
            #!${pkgs.runtimeShell}
            exec ${module.config.home.activationPackage}/activate
          '';
        };
      in rec {
        common = buildHomeManager "${self}/nixpkgs-config/common.nix";
        desktop = buildHomeManager "${self}/nixpkgs-config/desktop.nix";
      };

      hydraJobs = {
        configurations = nixpkgs.lib.mapAttrs' (name: config:
          nixpkgs.lib.nameValuePair name config.config.system.build.toplevel)
          self.nixosConfigurations;
        hmConfigurations = nixpkgs.lib.mapAttrs' (name: config:
          nixpkgs.lib.nameValuePair name config.activate)
          self.hmConfigurations;
      };
    };
}
