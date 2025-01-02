{
  description = "NixOS configuration with flakes";

  nixConfig.extra-substituters = [ "https://cache.thalheim.io" ];
  nixConfig.extra-trusted-public-keys = [
    "cache.thalheim.io-1:R7msbosLEZKrxk/lKxf9BTjOOH7Ax3H0Qj0/6wiHOgc="
  ];

  # To update all inputs:
  # $ nix flake update
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";

    data-mesher.url = "https://git.clan.lol/clan/data-mesher/archive/main.tar.gz";
    data-mesher.inputs.nixpkgs.follows = "nixpkgs";
    data-mesher.inputs.flake-parts.follows = "flake-parts";
    data-mesher.inputs.treefmt-nix.follows = "treefmt-nix";

    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";
    hercules-ci-effects.inputs.nixpkgs.follows = "nixpkgs";
    hercules-ci-effects.inputs.flake-parts.follows = "flake-parts";

    harmonia.url = "github:nix-community/harmonia";
    harmonia.inputs.nixpkgs.follows = "nixpkgs";
    harmonia.inputs.flake-parts.follows = "flake-parts";
    harmonia.inputs.treefmt-nix.follows = "treefmt-nix";

    nix-index-database.url = "github:nix-community/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";

    nix.url = "git+https://github.com/Mic92/nix-1?shallow=1";
    nix.inputs.nixpkgs.follows = "nixpkgs";
    nix.inputs.flake-parts.follows = "";
    nix.inputs.flake-compat.follows = "";
    nix.inputs.nixpkgs-regression.follows = "";
    nix.inputs.git-hooks-nix.follows = "";

    nixpkgs.url = "git+https://github.com/Mic92/nixpkgs?shallow=1";
    # for development
    #nixpkgs.url = "/home/joerg/git/nixpkgs";
    nur-packages.url = "github:Mic92/nur-packages";
    nur-packages.inputs.nixpkgs.follows = "nixpkgs";
    # for development
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    #retiolum.url = "git+https://git.thalheim.io/Mic92/retiolum";
    retiolum.url = "github:Mic92/retiolum";

    #spora.url = "github:krebs/spora";
    #spora.inputs.nixpkgs.follows = "nixpkgs";

    #nixos-hardware.url = "github:NixOS/nixos-hardware";
    nixos-hardware.url = "github:Mic92/nixos-hardware/framework-13-audio-improvements";

    systems.url = "github:nix-systems/default";

    buildbot-nix.url = "github:nix-community/buildbot-nix";
    buildbot-nix.inputs.nixpkgs.follows = "nixpkgs";
    buildbot-nix.inputs.flake-parts.follows = "flake-parts";
    buildbot-nix.inputs.treefmt-nix.follows = "treefmt-nix";
    buildbot-nix.inputs.hercules-ci-effects.follows = "";

    hyprspace.url = "github:hyprspace/hyprspace";
    hyprspace.inputs.flake-parts.follows = "flake-parts";
    hyprspace.inputs.nixpkgs.follows = "nixpkgs";

    nixos-facter-modules.url = "github:numtide/nixos-facter-modules";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    flake-registry.url = "github:NixOS/flake-registry";
    flake-registry.flake = false;

    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    #srvos.url = "github:numtide/srvos/impure-derivations";
    srvos.url = "github:numtide/srvos/dotfiles";
    srvos.inputs.nixpkgs.follows = "nixpkgs";

    clan-core.url = "https://git.clan.lol/clan/clan-core/archive/main.tar.gz";
    #clan-core.url = "path:///Users/joerg/git/clan-core";
    #clan-core.url = "git+https://git.clan.lol/clan/clan-core?ref=nixos-rebuild";
    #clan-core.url = "git+file:///home/joerg/work/clan/clan-core";
    clan-core.inputs.nixpkgs.follows = "nixpkgs";
    clan-core.inputs.sops-nix.follows = "sops-nix";
    clan-core.inputs.treefmt-nix.follows = "treefmt-nix";
    clan-core.inputs.disko.follows = "disko";
    clan-core.inputs.flake-parts.follows = "flake-parts";
    clan-core.inputs.nixos-facter-modules.follows = "nixos-facter-modules";
    clan-core.inputs.systems.follows = "systems";

    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";

    lanzaboote.url = "github:nix-community/lanzaboote";
    lanzaboote.inputs.nixpkgs.follows = "nixpkgs";
    lanzaboote.inputs.flake-parts.follows = "flake-parts";
    lanzaboote.inputs.flake-compat.follows = "";
    lanzaboote.inputs.pre-commit-hooks-nix.follows = "";

    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.systems.follows = "systems";

    nether.url = "github:Lassulus/nether";
    nether.inputs.clan-core.follows = "clan-core";
    nether.inputs.nixpkgs.follows = "nixpkgs";
    nether.inputs.treefmt-nix.follows = "treefmt-nix";
    nether.inputs.data-mesher.follows = "data-mesher";
    nether.inputs.flake-parts.follows = "flake-parts";

    #microvm.url = "github:astro/microvm.nix";
    #microvm.inputs.nixpkgs.follows = "nixpkgs";
    #microvm.inputs.flake-utils.follows = "flake-utils";
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } (
      {
        withSystem,
        self,
        config,
        ...
      }:
      {
        imports = [
          ./darwin/flake-module.nix
          ./machines/flake-module.nix
          ./home-manager/flake-module.nix
          ./terraform/flake-module.nix
          ./devshell/flake-module.nix
          ./pkgs/images/flake-module.nix
          ./pkgs/flake-module.nix
          inputs.hercules-ci-effects.flakeModule
          inputs.clan-core.flakeModules.default
        ];
        systems = [
          "x86_64-linux"
          "aarch64-linux"
          "aarch64-darwin"
        ];

        herculesCI = herculesCI: {
          onPush.default.outputs.effects.deploy = withSystem config.defaultEffectSystem (
            { pkgs, hci-effects, ... }:
            hci-effects.runIf (herculesCI.config.repo.branch == "main") (
              hci-effects.mkEffect {
                effectScript = ''
                  echo "${builtins.toJSON { inherit (herculesCI.config.repo) branch tag rev; }}"
                  ${pkgs.hello}/bin/hello
                '';
              }
            )
          );
        };

        perSystem =
          {
            inputs',
            self',
            lib,
            system,
            ...
          }:
          {
            # make pkgs available to all `perSystem` functions
            _module.args.pkgs = inputs'.nixpkgs.legacyPackages;

            checks =
              let
                machinesPerSystem = {
                  aarch64-linux = [
                    "blob64"
                    "matchbox"
                  ];
                  x86_64-linux = [
                    "eve"
                    "eva"
                    "turingmachine"
                    "bernie"
                  ];
                };
                nixosMachines = lib.mapAttrs' (n: lib.nameValuePair "nixos-${n}") (
                  lib.genAttrs (machinesPerSystem.${system} or [ ]) (
                    name: self.nixosConfigurations.${name}.config.system.build.toplevel
                  )
                );

                blacklistPackages = [
                  "install-iso"
                  "nspawn-template"
                  "netboot-pixie-core"
                  "netboot"
                ];
                packages = lib.mapAttrs' (n: lib.nameValuePair "package-${n}") (
                  lib.filterAttrs (n: _v: !(builtins.elem n blacklistPackages)) self'.packages
                );
                devShells = lib.mapAttrs' (n: lib.nameValuePair "devShell-${n}") self'.devShells;
                homeConfigurations = lib.mapAttrs' (
                  name: config: lib.nameValuePair "home-manager-${name}" config.activation-script
                ) (self'.legacyPackages.homeConfigurations or { });
              in
              nixosMachines // packages // devShells // homeConfigurations;
          };
        # CI
      }
    );
}
