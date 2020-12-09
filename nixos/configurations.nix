{ nixpkgs
, nixosSystem
, nur
, home-manager
, sops-nix
, retiolum
, nixos-hardware
, flake-registry
, bme680-mqtt
, nix-ld
}:
let
  defaultModules = [
    {
      nix.nixPath = [
        "home-manager=${home-manager}"
        "nixpkgs=${nixpkgs}"
        "nur=${nur}"
      ];
      nix.extraOptions = ''
        flake-registry = ${flake-registry}/flake-registry.json
      '';
      nix.registry = {
        home-manager.flake = home-manager;
        nixpkgs.flake = nixpkgs;
        nur.flake = nur;
        sops-nix.flake = sops-nix;
      };
      nixpkgs.overlays = [ nur.overlay ];
      imports = [
        ./modules/nix-daemon.nix
        ./modules/drone-deploy.nix
      ];
      #system.nixos.versionSuffix = "";
      documentation.info.enable = false;
    }
    retiolum.nixosModules.retiolum
    sops-nix.nixosModules.sops
  ];
  eveModules = defaultModules ++ [
    ./eve/configuration.nix
    {
      nixpkgs.overlays = [(self: super: {
        inherit retiolum;
      })];
    }
  ];
in {
  bernie = nixosSystem {
    system = "x86_64-linux";
    modules = defaultModules ++ [
      nixos-hardware.nixosModules.lenovo-thinkpad-x250
      ./bernie/configuration.nix
    ];
  };

  turingmachine = nixosSystem {
    system = "x86_64-linux";
    modules = defaultModules ++ [
      nixos-hardware.nixosModules.dell-xps-13-9380
      nix-ld.nixosModules.nix-ld
      ./turingmachine/configuration.nix
    ];
  };

  eddie = nixosSystem {
    system = "x86_64-linux";
    modules = defaultModules ++ [ ./eddie/configuration.nix ];
  };

  eve = nixosSystem {
    system = "x86_64-linux";
    modules = eveModules;
  };

  #rock = nixosSystem {
  #  system = "aarch64-linux";
  #  modules = defaultModules ++ [
  #    bme680-mqtt.nixosModules.bme680-mqtt
  #    ./rock/configuration.nix
  #  ];
  #};

  #matchbox = nixosSystem {
  #  system = "aarch64-linux";
  #  modules = defaultModules ++ [
  #    ./matchbox/configuration.nix
  #  ];
  #};

  #eve-vm = nixosSystem {
  #  system = "x86_64-linux";
  #  modules = eveModules ++ [
  #    "${nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix"
  #  ];
  #};

  eva = nixosSystem {
    system = "x86_64-linux";
    modules = defaultModules ++ [ ./eva/configuration.nix ];
  };
}
