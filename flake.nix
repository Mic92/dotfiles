{
  description = "NixOS configuration with flakes";

  nixConfig.extra-substituters = [ "https://cache.thalheim.io" ];
  nixConfig.extra-trusted-public-keys = [
    "cache.thalheim.io-1:R7msbosLEZKrxk/lKxf9BTjOOH7Ax3H0Qj0/6wiHOgc="
  ];

  # To update all inputs:
  # $ nix flake update
  inputs = {
    adios-flake.url = "github:Mic92/adios-flake";

    # Kept as a top-level input so upstream dependencies that use
    # flake-parts all share a single copy via follows.
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";

    hercules-ci-effects.url = "git+https://github.com/hercules-ci/hercules-ci-effects?shallow=1";
    hercules-ci-effects.inputs.nixpkgs.follows = "nixpkgs";
    hercules-ci-effects.inputs.flake-parts.follows = "flake-parts";

    harmonia.url = "github:nix-community/harmonia";
    harmonia.inputs.nixpkgs.follows = "nixpkgs";
    harmonia.inputs.treefmt-nix.follows = "treefmt-nix";
    harmonia.inputs.flake-parts.follows = "flake-parts";
    harmonia.inputs.crane.follows = "crane";
    harmonia.inputs.nix.follows = "nix";

    nix-index-database.url = "github:nix-community/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";

    noctalia-plugins.url = "github:Mic92/noctalia-plugins/nostr";
    noctalia-plugins.inputs.nixpkgs.follows = "nixpkgs";

    nix.url = "git+https://github.com/Mic92/nix-1?shallow=1";
    nix.inputs.nixpkgs.follows = "nixpkgs";
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
    data-mesher.inputs.treefmt-nix.follows = "treefmt-nix";
    data-mesher.inputs.flake-parts.follows = "flake-parts";

    buildbot-nix.url = "git+https://github.com/nix-community/buildbot-nix?shallow=1";
    buildbot-nix.inputs.nixpkgs.follows = "nixpkgs";
    buildbot-nix.inputs.treefmt-nix.follows = "treefmt-nix";
    buildbot-nix.inputs.flake-parts.follows = "flake-parts";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    flake-registry.url = "github:NixOS/flake-registry";
    flake-registry.flake = false;

    nix-darwin.url = "github:nix-darwin/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    strace-macos.url = "github:Mic92/strace-macos";
    strace-macos.inputs.nixpkgs.follows = "nixpkgs";
    strace-macos.inputs.treefmt-nix.follows = "treefmt-nix";
    strace-macos.inputs.flake-parts.follows = "flake-parts";

    forge-triage.url = "github:Mic92/forge-triage";
    forge-triage.inputs.nixpkgs.follows = "nixpkgs";

    srvos.url = "github:nix-community/srvos";
    srvos.inputs.nixpkgs.follows = "nixpkgs";

    clan-core.url = "git+https://git.clan.lol/clan/clan-core?ref=main";
    #clan-core.url = "path:///Users/joerg/git/clan-core";
    #clan-core.url = "git+https://git.clan.lol/clan/clan-core?ref=nixos-rebuild";
    #clan-core.url = "git+file:///home/joerg/work/clan/clan-core";
    clan-core.inputs.nixpkgs.follows = "nixpkgs";
    clan-core.inputs.sops-nix.follows = "sops-nix";
    clan-core.inputs.treefmt-nix.follows = "treefmt-nix";
    clan-core.inputs.data-mesher.follows = "data-mesher";
    clan-core.inputs.disko.follows = "disko";
    clan-core.inputs.flake-parts.follows = "flake-parts";
    clan-core.inputs.systems.follows = "systems";
    clan-core.inputs.nix-darwin.follows = "nix-darwin";

    mics-n8n-nodes.url = "github:Mic92/mics-n8n-nodes";
    mics-n8n-nodes.inputs.nixpkgs.follows = "nixpkgs";
    mics-n8n-nodes.inputs.treefmt-nix.follows = "treefmt-nix";
    mics-n8n-nodes.inputs.flake-parts.follows = "flake-parts";

    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    direnv-instant.url = "github:Mic92/direnv-instant";
    direnv-instant.inputs.nixpkgs.follows = "nixpkgs";
    direnv-instant.inputs.treefmt-nix.follows = "treefmt-nix";
    direnv-instant.inputs.flake-parts.follows = "flake-parts";

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

    opencrow.url = "github:pinpox/opencrow";
    opencrow.inputs.nixpkgs.follows = "nixpkgs";
    opencrow.inputs.treefmt-nix.follows = "treefmt-nix";

    llm-agents = {
      url = "github:numtide/llm-agents.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.blueprint.follows = "blueprint";
    };

    nix-diff-rs = {
      url = "github:Mic92/nix-diff-rs";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.flake-parts.follows = "flake-parts";
    };

    nix-tree-rs = {
      url = "github:Mic92/nix-tree-rs";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.flake-parts.follows = "flake-parts";
    };

    nix-casks = {
      url = "github:atahanyorganci/nix-casks/archive";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.flake-parts.follows = "flake-parts";
    };

    niks3 = {
      url = "github:Mic92/niks3";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.flake-parts.follows = "flake-parts";
    };

    freelancer-toolbox.url = "github:numtide/freelancer-toolbox";
    freelancer-toolbox.inputs.nixpkgs.follows = "nixpkgs";
    freelancer-toolbox.inputs.flake-parts.follows = "flake-parts";
    freelancer-toolbox.inputs.treefmt-nix.follows = "treefmt-nix";

    mics-skills = {
      url = "github:Mic92/mics-skills";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.flake-parts.follows = "flake-parts";
    };

    #microvm.url = "github:astro/microvm.nix";
    #microvm.inputs.nixpkgs.follows = "nixpkgs";
    #microvm.inputs.flake-utils.follows = "flake-utils";
  };

  outputs =
    inputs@{ adios-flake, self, ... }:
    adios-flake.lib.mkFlake {
      inherit inputs self;
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      modules = [
        ./nixosModules/openldap/flake-module.nix
        ./home-manager/flake-module.nix
        ./home-manager/modules/neovim/flake-module.nix
        ./devshell/flake-module.nix
        ./pkgs/flake-module.nix
        ./openwrt/flake-module.nix
        ./checks/flake-module.nix
      ];
      flake =
        let
          # Evaluate clan outside mkFlake since it produces system-agnostic outputs
          clan = import ./machines/inventory.nix self;
        in
        {
          inherit (clan.config)
            nixosConfigurations
            darwinConfigurations
            darwinModules
            clanInternals
            ;

          clan = clan.config;

          nixosModules.default = ./nixosModules/default.nix;
          nixosModules.authelia = ./nixosModules/authelia;
        };
    };
}
