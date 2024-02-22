{
  description = "NixOS configuration with flakes";

  nixConfig.extra-substituters = [ "https://cache.thalheim.io" ];
  nixConfig.extra-trusted-public-keys = [ "cache.thalheim.io-1:R7msbosLEZKrxk/lKxf9BTjOOH7Ax3H0Qj0/6wiHOgc=" ];

  # To update all inputs:
  # $ nix flake update
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";

    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";
    hercules-ci-effects.inputs.nixpkgs.follows = "nixpkgs";
    hercules-ci-effects.inputs.flake-parts.follows = "flake-parts";

    nix-index-database.url = "github:nix-community/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:Mic92/nixpkgs/main";
    # for development
    #nixpkgs.url = "/home/joerg/git/nixpkgs";
    nur-packages.url = "github:Mic92/nur-packages";
    nur-packages.inputs.nixpkgs.follows = "nixpkgs";
    # for development
    #sops-nix.url = "/home/joerg/git/sops-nix";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    sops-nix.inputs.nixpkgs-stable.follows = "";

    bme680-mqtt.url = "github:Mic92/bme680-mqtt";
    bme680-mqtt.inputs.flake-parts.follows = "flake-parts";
    bme680-mqtt.inputs.nixpkgs.follows = "nixpkgs";

    #retiolum.url = "git+https://git.thalheim.io/Mic92/retiolum";
    retiolum.url = "github:Mic92/retiolum";

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    buildbot-nix.url = "github:Mic92/buildbot-nix";
    buildbot-nix.inputs.nixpkgs.follows = "nixpkgs";
    buildbot-nix.inputs.flake-parts.follows = "flake-parts";
    buildbot-nix.inputs.treefmt-nix.follows = "treefmt-nix";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    astro-nvim.url = "github:AstroNvim/AstroNvim";
    astro-nvim.flake = false;

    flake-registry.url = "github:NixOS/flake-registry";
    flake-registry.flake = false;

    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    nix-ld-rs.url = "github:nix-community/nix-ld-rs";
    nix-ld-rs.inputs.nixpkgs.follows = "nixpkgs";
    nix-ld-rs.inputs.flake-utils.follows = "flake-utils";

    srvos.url = "github:numtide/srvos";
    srvos.inputs.nixpkgs.follows = "nixpkgs";

    clan-core.url = "git+https://git.clan.lol/clan/clan-core";
    #clan-core.url = "git+file:///home/joerg/work/clan/clan-core?ref=main";
    clan-core.inputs.nixpkgs.follows = "nixpkgs";
    clan-core.inputs.sops-nix.follows = "sops-nix";
    clan-core.inputs.treefmt-nix.follows = "treefmt-nix";
    clan-core.inputs.disko.follows = "disko";
    clan-core.inputs.flake-parts.follows = "flake-parts";
    clan-core.inputs.nixos-generators.follows = "nixos-generators";

    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";

    lanzaboote.url = "github:nix-community/lanzaboote";
    lanzaboote.inputs.flake-parts.follows = "flake-parts";
    lanzaboote.inputs.flake-utils.follows = "flake-utils";
    lanzaboote.inputs.flake-compat.follows = "";
    lanzaboote.inputs.pre-commit-hooks-nix.follows = "";

    fast-flake-update.url = "github:Mic92/fast-flake-update";
    fast-flake-update.inputs.nixpkgs.follows = "nixpkgs";
    fast-flake-update.inputs.flake-parts.follows = "flake-parts";
    fast-flake-update.inputs.treefmt-nix.follows = "treefmt-nix";

    flake-utils.url = "github:numtide/flake-utils";

    nixos-wiki.url = "github:Mic92/nixos-wiki-infra";
    nixos-wiki.inputs.nixpkgs.follows = "nixpkgs";
    nixos-wiki.inputs.flake-parts.follows = "flake-parts";
    nixos-wiki.inputs.treefmt-nix.follows = "treefmt-nix";
    nixos-wiki.inputs.disko.follows = "disko";
    nixos-wiki.inputs.sops-nix.follows = "sops-nix";
    nixos-wiki.inputs.srvos.follows = "srvos";

    nether.url = "github:Lassulus/nether";
    nether.inputs.clan-core.follows = "clan-core";
    nether.inputs.nixpkgs.follows = "nixpkgs";

    #microvm.url = "github:astro/microvm.nix";
    #microvm.inputs.nixpkgs.follows = "nixpkgs";
    #microvm.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = inputs @ { self, flake-parts, nixpkgs, ... }:
    (flake-parts.lib.evalFlakeModule
      { inherit inputs; }
      ({ withSystem, config, ... }: {
        imports = [
          ./nixos/flake-module.nix
          ./nixos/images/flake-module.nix
          ./home-manager/flake-module.nix
          ./terraform/flake-module.nix
          ./devshell/flake-module.nix
          inputs.hercules-ci-effects.flakeModule
        ];
        systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];

        herculesCI = herculesCI: {
          onPush.default.outputs.effects.deploy = withSystem config.defaultEffectSystem ({ pkgs, hci-effects, ... }:
            hci-effects.runIf (herculesCI.config.repo.branch == "main") (hci-effects.mkEffect {
              effectScript = ''
                ${pkgs.hello}/bin/hello
              '';
            })
          );
        };

        perSystem = { config, inputs', self', lib, system, ... }: {
          # make pkgs available to all `perSystem` functions
          _module.args.pkgs = inputs'.nixpkgs.legacyPackages;

          formatter = config.treefmt.build.wrapper;

          checks =
            let
              nixosMachines = lib.mapAttrs' (name: config: lib.nameValuePair "nixos-${name}" config.config.system.build.toplevel) ((lib.filterAttrs (_: config: config.pkgs.system == system)) self.nixosConfigurations);
              blacklistPackages = [ "install-iso" "nspawn-template" "netboot-pixie-core" "netboot" ];
              packages = lib.mapAttrs' (n: lib.nameValuePair "package-${n}") (lib.filterAttrs (n: _v: !(builtins.elem n blacklistPackages)) self'.packages);
              devShells = lib.mapAttrs' (n: lib.nameValuePair "devShell-${n}") self'.devShells;
              homeConfigurations = lib.mapAttrs' (name: config: lib.nameValuePair "home-manager-${name}" config.activation-script) (self'.legacyPackages.homeConfigurations or { });
            in
            nixosMachines // packages // devShells // homeConfigurations;
        };
        # CI
      })).config.flake;
}
