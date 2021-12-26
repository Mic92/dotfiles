{ self
, nixpkgs
, nixpkgs-systemd
, nur
, home-manager
, sops-nix
, retiolum
, flake-registry
, bme680-mqtt
, inputs
, nixos-hardware
, miniond
, ...
}:
let
  nixosSystem = nixpkgs.lib.makeOverridable nixpkgs.lib.nixosSystem;

  defaultModules = [
    # make flake inputs accessiable in NixOS
    { _module.args.inputs = inputs; }
    {
      imports = [
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
        retiolum.nixosModules.ca
        sops-nix.nixosModules.sops
      ];
    }
  ];
in
{
  bernie = nixosSystem {
    system = "x86_64-linux";
    modules = defaultModules ++ [
      nixos-hardware.nixosModules.lenovo-thinkpad-x250
      home-manager.nixosModules.home-manager
      ./bernie/configuration.nix
    ];
  };

  turingmachine = nixosSystem {
    system = "x86_64-linux";
    modules = defaultModules ++ [
      ./turingmachine/configuration.nix
      ({ pkgs, ... }: {
        systemd.package = (import nixpkgs-systemd {
          inherit (pkgs) system;
        }).systemd;
      })
      nixos-hardware.nixosModules.lenovo-thinkpad-x13
      nixos-hardware.nixosModules.dell-xps-13-9380
      inputs.nix-ld.nixosModules.nix-ld
      #inputs.envfs.nixosModules.envfs
    ];
  };

  eve = nixosSystem {
    system = "x86_64-linux";
    modules = defaultModules ++ [
      ./eve/configuration.nix
    ];
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

  eva = nixosSystem {
    system = "x86_64-linux";
    modules = defaultModules ++ [
      ./eva/configuration.nix
    ];
  };

  cloudlab-node = nixosSystem {
    system = "x86_64-linux";
    modules = defaultModules ++ [
      miniond.nixosModule
      {
        hardware.emulab.enable = true;
        loader.grub = {
          enable = true;
          version = 2;
          # what about nvme?
          device = "/dev/sda";
        };

        fileSystems."/boot" = {
          device = "/dev/disk/by-label/NIXOS_BOOT";
          fsType = "ext4";
        };
        fileSystems."/" = {
          device = "/dev/disk/by-label/NIXOS_ROOT";
          fsType = "ext4";
        };
      }
    ];
  };
}
