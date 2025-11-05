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
    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";
    hercules-ci-effects.inputs.nixpkgs.follows = "nixpkgs";
    hercules-ci-effects.inputs.flake-parts.follows = "flake-parts";

    harmonia.url = "github:nix-community/harmonia/harmonia-next";
    harmonia.inputs.nixpkgs.follows = "nixpkgs";
    harmonia.inputs.flake-parts.follows = "flake-parts";
    harmonia.inputs.treefmt-nix.follows = "treefmt-nix";
    harmonia.inputs.crane.follows = "crane";

    nix-index-database.url = "github:nix-community/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";

    nix.url = "git+https://github.com/Mic92/nix-1?shallow=1";
    nix.inputs.nixpkgs.follows = "nixpkgs";
    nix.inputs.flake-parts.follows = "";
    nix.inputs.flake-compat.follows = "";
    nix.inputs.nixpkgs-regression.follows = "";
    nix.inputs.git-hooks-nix.follows = "";
    nix.inputs.nixpkgs-23-11.follows = "";

    nixpkgs.url = "git+https://github.com/Mic92/nixpkgs?shallow=1&ref=main";
    # for development
    #nixpkgs.url = "/home/joerg/git/nixpkgs";
    nur-packages.url = "github:Mic92/nur-packages";
    nur-packages.inputs.nixpkgs.follows = "nixpkgs";
    # for development
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    #retiolum.url = "git+https://git.thalheim.io/Mic92/retiolum";
    #retiolum.url = "github:Mic92/retiolum";
    retiolum.url = "git+https://github.com/Mic92/retiolum?shallow=1";
    retiolum.inputs.nixpkgs.follows = "nixpkgs";
    retiolum.inputs.nix-darwin.follows = "nix-darwin";

    #spora.url = "github:krebs/spora";
    #spora.inputs.nixpkgs.follows = "nixpkgs";

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    systems.url = "github:nix-systems/default";

    data-mesher.url = "git+https://git.clan.lol/clan/data-mesher?shallow=1";
    data-mesher.inputs.nixpkgs.follows = "nixpkgs";
    data-mesher.inputs.flake-parts.follows = "flake-parts";
    data-mesher.inputs.treefmt-nix.follows = "treefmt-nix";

    buildbot-nix.url = "git+https://github.com/nix-community/buildbot-nix?shallow=1&ref=failed-statuses";
    buildbot-nix.inputs.nixpkgs.follows = "nixpkgs";
    buildbot-nix.inputs.flake-parts.follows = "flake-parts";
    buildbot-nix.inputs.treefmt-nix.follows = "treefmt-nix";
    buildbot-nix.inputs.hercules-ci-effects.follows = "";

    hyprspace.url = "github:Mic92/hyprspace/fix-darwin";
    hyprspace.inputs.flake-parts.follows = "flake-parts";
    hyprspace.inputs.nixpkgs.follows = "nixpkgs";

    nixos-facter-modules.url = "github:numtide/nixos-facter-modules";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    flake-registry.url = "github:NixOS/flake-registry";
    flake-registry.flake = false;

    nix-darwin.url = "github:nix-darwin/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    srvos.url = "github:nix-community/srvos";
    srvos.inputs.nixpkgs.follows = "nixpkgs";

    clan-core.url = "https://git.clan.lol/clan/clan-core/archive/disko-fix.tar.gz";

    #clan-core.url = "path:///Users/joerg/git/clan-core";
    #clan-core.url = "git+https://git.clan.lol/clan/clan-core?ref=nixos-rebuild";
    #clan-core.url = "git+file:///home/joerg/work/clan/clan-core";

    n8n-nodes-caldav.url = "github:Mic92/n8n-nodes-caldav";
    n8n-nodes-caldav.inputs.nixpkgs.follows = "nixpkgs";
    n8n-nodes-caldav.inputs.flake-parts.follows = "flake-parts";

    clan-core.inputs.nixpkgs.follows = "nixpkgs";
    clan-core.inputs.sops-nix.follows = "sops-nix";
    clan-core.inputs.treefmt-nix.follows = "treefmt-nix";
    clan-core.inputs.disko.follows = "disko";
    clan-core.inputs.flake-parts.follows = "flake-parts";
    clan-core.inputs.nixos-facter-modules.follows = "nixos-facter-modules";
    clan-core.inputs.systems.follows = "systems";
    clan-core.inputs.nix-darwin.follows = "nix-darwin";

    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    direnv-instant.url = "github:Mic92/direnv-instant";
    direnv-instant.inputs.nixpkgs.follows = "nixpkgs";
    direnv-instant.inputs.flake-parts.follows = "flake-parts";
    direnv-instant.inputs.treefmt-nix.follows = "treefmt-nix";

    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";

    flake-fmt.url = "github:Mic92/flake-fmt";
    flake-fmt.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.systems.follows = "systems";

    crane.url = "github:ipetkov/crane";

    blueprint = {
      url = "github:numtide/blueprint";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.systems.follows = "systems";
    };

    nix-ai-tools = {
      url = "github:numtide/nix-ai-tools";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.blueprint.follows = "blueprint";
    };

    nix-diff-rs = {
      url = "github:Mic92/nix-diff-rs";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
      inputs.treefmt-nix.follows = "treefmt-nix";
    };

    nix-tree-rs = {
      url = "github:Mic92/nix-tree-rs";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
      inputs.treefmt-nix.follows = "treefmt-nix";
    };

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
          ./machines/flake-module.nix
          ./nixosModules/openldap/flake-module.nix
          ./home-manager/flake-module.nix
          ./terraform/flake-module.nix
          ./devshell/flake-module.nix
          ./pkgs/images/flake-module.nix
          ./pkgs/flake-module.nix
          ./openwrt/flake-module.nix
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
                    "dorits-laptop"
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
