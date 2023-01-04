{ self, inputs, ... }:
let
  inherit (inputs.nixpkgs) lib;
  inherit (inputs) nixpkgs;

  nixosSystem = args:
    (lib.makeOverridable lib.nixosSystem)
      (lib.recursiveUpdate args {
        modules =
          args.modules
          ++ [
            {
              config.nixpkgs.pkgs = lib.mkDefault args.pkgs;
              config.nixpkgs.localSystem = lib.mkDefault args.pkgs.stdenv.hostPlatform;
            }
          ];
      });

  defaultModules = [
    # make flake inputs accessiable in NixOS
    {
      _module.args.self = self;
      _module.args.inputs = self.inputs;
    }
    ({ pkgs, ... }: {
      nix.nixPath = [
        "nixpkgs=${pkgs.path}"
        "home-manager=${inputs.home-manager}"
        "nur=${inputs.nur}"
      ];

      #nix.extraOptions = let
      #  registry = pkgs.runCommand "flake-registry.json" {} ''
      #    jq 'setpath(;)' < ${flake-registry}/flake-registry.json > $out
      #  '';
      #in ''
      #  flake-registry = ${registry}/flake-registry.json
      #'';
      nix.extraOptions = ''
        flake-registry = ${inputs.flake-registry}/flake-registry.json
      '';
      srvos.flake = self;
      documentation.info.enable = false;
      services.envfs.enable = true;

      imports = [
        ./modules/acme.nix
        ./modules/nix-daemon.nix
        ./modules/minimal-docs.nix
        ./modules/i18n.nix
        ./modules/sshd
        ./modules/zfs.nix
        inputs.nur.nixosModules.nur

        inputs.srvos.nixosModules.common
        inputs.srvos.nixosModules.mixins-telegraf
        { networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [ 9273 ]; }

        ./modules/retiolum.nix
        ./modules/update-prefetch.nix
        inputs.retiolum.nixosModules.retiolum
        inputs.retiolum.nixosModules.ca

        inputs.sops-nix.nixosModules.sops
      ];
    })
  ];
in
{
  flake.nixosConfigurations = {
    bernie = nixosSystem {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      modules =
        defaultModules
        ++ [
          inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x13
          inputs.home-manager.nixosModules.home-manager
          inputs.srvos.nixosModules.desktop
          ./bernie/configuration.nix
        ];
    };

    turingmachine = nixosSystem {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      modules =
        defaultModules
        ++ [
          ./turingmachine/configuration.nix
          inputs.nixos-hardware.nixosModules.framework
          inputs.nix-index-database.nixosModules.nix-index
          inputs.hyprland.nixosModules.default

          inputs.srvos.nixosModules.desktop

          #self.inputs.lanzaboote.nixosModules.lanzaboote
          #({pkgs, ...}: {
          #  programs.hyprland.enable = true;
          #  environment.systemPackages = [
          #    hyprland.packages.${pkgs.system}.waybar-hyprland
          #  ];
          #})
          # For testing systemd
          #({pkgs, ...}: {
          #  #systemd.package = self.inputs.nixpkgs-systemd.legacyPackages.${pkgs.system}.systemd;
          #  systemd.package = pkgs.systemd.overrideAttrs (old: {
          #    src = self.inputs.systemd;
          #  });
          #})
        ];
    };

    eve = nixosSystem {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      modules =
        defaultModules
        ++ [
          ./eve/configuration.nix

          inputs.srvos.nixosModules.server
          inputs.srvos.nixosModules.mixins-nginx

          "${inputs.harmonia}/module.nix"
        ];
    };

    rock = nixosSystem {
      pkgs = nixpkgs.legacyPackages.aarch64-linux;
      modules =
        defaultModules
        ++ [
          inputs.srvos.nixosModules.server
          inputs.bme680-mqtt.nixosModules.bme680-mqtt
          ./rock/configuration.nix
        ];
    };

    blob64 = nixosSystem {
      pkgs = nixpkgs.legacyPackages.aarch64-linux;
      modules =
        defaultModules
        ++ [
          inputs.srvos.nixosModules.server
          ./blob64/configuration.nix
        ];
    };

    matchbox = nixosSystem {
      pkgs = nixpkgs.legacyPackages.aarch64-linux;
      modules = defaultModules ++ [
        inputs.srvos.nixosModules.server
        ./matchbox/configuration.nix
      ];
    };

    eva = nixosSystem {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      modules =
        defaultModules
        ++ [
          inputs.srvos.nixosModules.server
          inputs.srvos.nixosModules.mixins-nginx
          inputs.srvos.nixosModules.mixins-systemd-boot
          inputs.disko.nixosModules.disko
          { boot.loader.efi.canTouchEfiVariables = true; }
          ./eva/configuration.nix
        ];
    };
  };
}
