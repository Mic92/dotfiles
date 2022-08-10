{self, ...}: let
  inherit
    (self.inputs)
    nixpkgs
    retiolum
    sops-nix
    home-manager
    nur
    flake-registry
    nixos-hardware
    nix-ld
    bme680-mqtt
    ;
  nixosSystem = nixpkgs.lib.makeOverridable nixpkgs.lib.nixosSystem;

  defaultModules = [
    # make flake inputs accessiable in NixOS
    {
      _module.args.self = self;
      _module.args.inputs = self.inputs;
    }
    {
      imports = [
        ({pkgs, ...}: {
          nix.nixPath = [
            "nixpkgs=${pkgs.path}"
            "home-manager=${home-manager}"
            "nur=${nur}"
          ];
          nix.extraOptions = ''
            flake-registry = ${flake-registry}/flake-registry.json
          '';
          documentation.info.enable = false;
          nixpkgs.config.packageOverrides = pkgs: {
            inherit retiolum;
          };
        })
        ./modules/upgrade-diff.nix
        ./modules/nix-daemon.nix
        ./modules/minimal-docs.nix
        ./modules/i18n.nix
        nur.nixosModules.nur

        ./modules/retiolum.nix
        retiolum.nixosModules.retiolum
        retiolum.nixosModules.ca

        sops-nix.nixosModules.sops
      ];
    }
  ];
in {
  flake.nixosConfigurations = {
    bernie = nixosSystem {
      system = "x86_64-linux";
      modules =
        defaultModules
        ++ [
          nixos-hardware.nixosModules.lenovo-thinkpad-x13
          home-manager.nixosModules.home-manager
          ./bernie/configuration.nix
        ];
    };

    turingmachine = nixosSystem {
      system = "x86_64-linux";
      modules =
        defaultModules
        ++ [
          ./turingmachine/configuration.nix
          nixos-hardware.nixosModules.framework
          nix-ld.nixosModules.nix-ld
          #inputs.envfs.nixosModules.envfs
          # For testing systemd
          #({pkgs, ...}: {
          #  systemd.package = self.inputs.nixpkgs-systemd.legacyPackages.${pkgs.system}.systemd;
          #})
        ];
    };

    eve = nixosSystem {
      system = "x86_64-linux";
      modules =
        defaultModules
        ++ [
          ./eve/configuration.nix
        ];
    };

    rock = nixosSystem {
      system = "aarch64-linux";
      modules =
        defaultModules
        ++ [
          bme680-mqtt.nixosModules.bme680-mqtt
          ./rock/configuration.nix
        ];
    };

    blob64 = nixosSystem {
      system = "aarch64-linux";
      modules =
        defaultModules
        ++ [
          ./blob64/configuration.nix
        ];
    };

    matchbox = nixosSystem {
      system = "aarch64-linux";
      modules =
        defaultModules
        ++ [
          ./matchbox/configuration.nix
        ];
    };

    eva = nixosSystem {
      system = "x86_64-linux";
      modules =
        defaultModules
        ++ [
          ./eva/configuration.nix
        ];
    };
  };
}
