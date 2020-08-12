{ nixpkgs
, nixosSystem
, nur
, home-manager
, sops-nix
, retiolum
, nixos-hardware
, choose-place
}:
let
  defaultModules = [
    {
      nix.nixPath = [
        "home-manager=${home-manager}"
        "nixpkgs=${nixpkgs}"
        "nur=${nur}"
      ];
      nixpkgs.overlays = [ nur.overlay ];
      imports = [
        ./modules/nix-daemon.nix
      ];
      #system.nixos.versionSuffix = "";
    }
    retiolum.nixosModules.retiolum
    sops-nix.nixosModules.sops
  ];
  eveModules = defaultModules ++ [
    ./eve/configuration.nix
    {
      nixpkgs.overlays = [(self: super: {
        choose-place = super.callPackage "${choose-place}" {};
        inherit retiolum;
      })];
    }
  ];
in {
  turingmachine = nixosSystem {
    system = "x86_64-linux";
    modules = defaultModules ++ [
      nixos-hardware.nixosModules.dell-xps-13-9380
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
