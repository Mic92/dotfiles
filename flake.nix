{
  description = "NixOS configuration with flakes";

  nixConfig.extra-substituters = [
    "https://mic92.cachix.org"
  ];
  nixConfig.extra-trusted-public-keys = [
    "mic92.cachix.org-1:gi8IhgiT3CYZnJsaW7fxznzTkMUOn1RY4GmXdT/nXYQ="
  ];

  # To update all inputs:
  # $ nix flake update
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    #systemd.url = "git+file:///home/joerg/git/systemd";
    #systemd.flake = false;

    nixos-generators = {
      #url = "github:nix-community/nixos-generators";
      url = "github:Mic92/nixos-generators/fedf7136f27490402fe8ab93e67fafae80513e9b";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:Mic92/nixpkgs/main";
    # for development
    #nixpkgs.url = "/home/joerg/git/nixpkgs";
    nur.url = "github:nix-community/NUR";
    # for development
    #sops-nix.url = "/home/joerg/git/sops-nix";
    sops-nix.url = "github:Mic92/sops-nix";

    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    bme680-mqtt.url = "github:Mic92/bme680-mqtt";
    bme680-mqtt.inputs.utils.follows = "flake-utils";
    bme680-mqtt.inputs.nixpkgs.follows = "nixpkgs";

    retiolum.url = "git+https://git.thalheim.io/Mic92/retiolum";

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    doom-emacs.url = "github:doomemacs/doomemacs/master";
    doom-emacs.flake = false;

    flake-registry.url = "github:NixOS/flake-registry";
    flake-registry.flake = false;

    nix-ld.url = "github:Mic92/nix-ld";
    nix-ld.inputs.nixpkgs.follows = "nixpkgs";
    nix-ld.inputs.utils.follows = "flake-utils";

    envfs.url = "github:Mic92/envfs";
    envfs.inputs.nixpkgs.follows = "nixpkgs";
    envfs.inputs.utils.follows = "flake-utils";

    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    deploykit.url = "github:numtide/deploykit";
    deploykit.inputs.nixpkgs.follows = "nixpkgs";
    deploykit.inputs.flake-parts.follows = "flake-parts";
  };

  #outputs = {...} @ args: import ./outputs.nix args;
  outputs = {
    self,
    flake-parts,
    ...
  }:
    (flake-parts.lib.evalFlakeModule
      {inherit self;}
      {
        imports = [
          ./nixos/configurations.nix
          ./nixos/images/default.nix
          ./nixpkgs-config/homes.nix
          ./terraform/flake-module.nix
          ./nixos/modules/xonsh/flake-module.nix
          ./shell.nix
          ./ci.nix
        ];
        systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin"];
        perSystem = {inputs', ...}: {
          # make pkgs available to all `perSystem` functions
          _module.args.pkgs = inputs'.nixpkgs.legacyPackages;
        };
      })
    .config
    .flake;
}
