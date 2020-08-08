{ nixpkgs
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
      #system.nixos.versionSuffix = "";
    }
    retiolum.nixosModules.retiolum
    sops-nix.nixosModules.sops
  ];
in {
  turingmachine = nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = defaultModules ++ [
      nixos-hardware.nixosModules.dell-xps-13-9380
      ./turingmachine/configuration.nix
    ];
  };

  eddie = nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = defaultModules ++ [ ./eddie/configuration.nix ];
  };

  eve = nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = defaultModules ++ [
      ./eve/configuration.nix
      {
        nixpkgs.overlays = [(self: super: {
          choose-place = super.callPackage "${choose-place}" {};
        })];
      }
    ];
  };

  eva = nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = defaultModules ++ [ ./eva/configuration.nix ];
  };
}
