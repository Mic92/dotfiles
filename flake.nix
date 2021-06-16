{
  description = "NixOS configuration with flakes";

  # To update all inputs:
  # $ nix flake update
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    lambda-pirate.url = "github:pogobanane/lambda-pirate/fix-eval";
    lambda-pirate.inputs.nixpkgs.follows = "nixpkgs";
    lambda-pirate.inputs.flake-utils.follows = "flake-utils";

    nixpkgs.url = "github:Mic92/nixpkgs/master";
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

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    #doom-emacs.url = "github:hlissner/doom-emacs";
    doom-emacs.url = "github:Mic92/doom-emacs/org-msg";
    doom-emacs.flake = false;

    #nix-doom-emacs.url = "github:vlaci/nix-doom-emacs";
    nix-doom-emacs.url = "github:Mic92/nix-doom-emacs";
    nix-doom-emacs.inputs.doom-emacs.follows = "doom-emacs";
    nix-doom-emacs.inputs.flake-utils.follows = "flake-utils";
    nix-doom-emacs.inputs.nixpkgs.follows = "nixpkgs";

    flake-registry.url = "github:NixOS/flake-registry";
    flake-registry.flake = false;

    nix-ld.url = "github:Mic92/nix-ld";
    nix-ld.inputs.nixpkgs.follows = "nixpkgs";
    nix-ld.inputs.utils.follows = "flake-utils";

    envfs.url = "github:Mic92/envfs";
    envfs.inputs.nixpkgs.follows = "nixpkgs";
    envfs.inputs.utils.follows = "flake-utils";

    hercules-ci.url = "github:hercules-ci/hercules-ci-agent/nixUnstable-compatibility";
    hercules-ci.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { ... } @ args: import ./outputs.nix args;
}
