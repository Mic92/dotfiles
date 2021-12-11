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
, envfs
, vmsh
}:
let
  defaultModules = [
    ({ pkgs, ... }: {
      nix.nixPath = [
        "nixpkgs=${pkgs.path}"
        "home-manager=${home-manager}"
        "nur=${nur}"
      ];
      nix.extraOptions = ''
        flake-registry = ${flake-registry}/flake-registry.json
      '';
      nixpkgs.overlays = [ nur.overlay ];
      documentation.info.enable = false;
      nixpkgs.config.packageOverrides = pkgs: {
        inherit retiolum;
      };
    })
    ./modules/upgrade-diff.nix
    ./modules/nix-daemon.nix
    retiolum.nixosModules.retiolum
    sops-nix.nixosModules.sops
  ];
  eveModules = defaultModules ++ [ ./eve/configuration.nix ];
in
{
  bernie = nixpkgs.lib.makeOverridable nixosSystem {
    system = "x86_64-linux";
    modules = defaultModules ++ [
      nixos-hardware.nixosModules.lenovo-thinkpad-x250
      home-manager.nixosModules.home-manager
      ./bernie/configuration.nix
    ];
  };

  turingmachine = nixpkgs.lib.makeOverridable nixosSystem {
    system = "x86_64-linux";
    modules = defaultModules ++ [
      #nixos-hardware.nixosModules.dell-xps-13-9380
      nixos-hardware.nixosModules.lenovo-thinkpad-x13
      nix-ld.nixosModules.nix-ld
      #envfs.nixosModules.envfs
      ./turingmachine/configuration.nix
    ];
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

  matchbox = nixosSystem {
    system = "aarch64-linux";
    modules = defaultModules ++ [
      ./matchbox/configuration.nix
    ];
  };

  #eve-vm = nixosSystem {
  #  system = "x86_64-linux";
  #  modules = eveModules ++ [
  #    "${nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix"
  #  ];
  #};

  eva = nixpkgs.lib.makeOverridable nixosSystem {
    system = "x86_64-linux";
    modules = defaultModules ++ [ ./eva/configuration.nix ];
  };
}
