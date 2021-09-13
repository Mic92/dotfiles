{
  description = "NixOS configuration with flakes";

  # To update all inputs:
  # $ nix flake update
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    fenix.url = "github:nix-community/fenix";
    fenix.inputs.nixpkgs.follows = "nixpkgs";
    lambda-pirate.url = "github:pogobanane/lambda-pirate/fix-eval";
    lambda-pirate.inputs.nixpkgs.follows = "nixpkgs";
    lambda-pirate.inputs.flake-utils.follows = "flake-utils";
    lambda-pirate.inputs.fenix.follows = "fenix";

    vmsh.url = "github:Mic92/vmsh";
    vmsh.inputs.flake-utils.follows = "flake-utils";
    vmsh.inputs.nixpkgs.follows = "nixpkgs";
    vmsh.inputs.fenix.follows = "fenix";

    nixpkgs.url = "github:Mic92/nixpkgs/main";
    nixpkgs-stable.url = "github:Mic92/nixpkgs/release-21.05-backports";

    #nixpkgs-systemd.url = "github:Mic92/nixpkgs/systemd-stable-update";
    nixpkgs-systemd.url = "github:Mic92/nixpkgs/systemd";
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

    #krops.url = "github:krebs/krops";
    #krops.flake = false;
    krops.url = "github:Mic92/krops";
    krops.inputs.flake-utils.follows = "flake-utils";
    krops.inputs.nixpkgs.follows = "nixpkgs";

    retiolum.url = "git+https://git.thalheim.io/Mic92/retiolum";

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    #home-manager.url = "github:nix-community/home-manager";
    home-manager.url = "github:Mic92/home-manager/nixos-option-test";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    doom-emacs.url = "github:hlissner/doom-emacs/develop";
    doom-emacs.flake = false;

    emacs-overlay.url = "github:nix-community/emacs-overlay";

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

    hercules-ci.url = "github:hercules-ci/hercules-ci-agent/hercules-ci-agent-0.8.2";
    hercules-ci.inputs.nixpkgs.follows = "nixpkgs";
    hercules-ci.inputs.nix-darwin.follows = "nix-darwin";
  };

  outputs = { ... } @ args: import ./outputs.nix args;
}
