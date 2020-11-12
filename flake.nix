{
  description = "NixOS configuration with flakes";

  # To update all inputs:
  # $ nix flake update --recreate-lock-file
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    nixpkgs.url = "github:Mic92/nixpkgs/master";
    nixpkgs-systemd.url = "github:Mic92/nixpkgs/systemd";
    # for development
    #nixpkgs.url = "/home/joerg/git/nixpkgs";
    nur.url = "github:nix-community/NUR";
    sops-nix.url = "github:Mic92/sops-nix";

    bme680-mqtt.url = "github:Mic92/bme680-mqtt";
    bme680-mqtt.inputs.utils.follows = "flake-utils";
    bme680-mqtt.inputs.nixpkgs.follows = "nixpkgs";

    #krops.url = "github:krebs/krops";
    #krops.flake = false;
    krops.url = "github:Mic92/krops";
    krops.inputs.flake-utils.follows = "flake-utils";
    krops.inputs.nixpkgs.follows = "nixpkgs";
    retiolum.url = "git+https://git.thalheim.io/Mic92/retiolum";
    # for development
    #sops-nix.url = "/home/joerg/git/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    nixos-hardware.url = "github:Mic92/nixos-hardware/master";
    home-manager.url = "github:rycee/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    doom-emacs.url = "github:hlissner/doom-emacs";
    #doom-emacs.url = "github:Mic92/doom-emacs/nix";
    doom-emacs.flake = false;

    nix-doom-emacs.url = "github:vlaci/nix-doom-emacs";
    nix-doom-emacs.flake = false;

    flake-registry.url = "github:NixOS/flake-registry";
    flake-registry.flake = false;
  };

  outputs = { self
            , nixpkgs
            , nixpkgs-systemd
            , nixos-hardware
            , sops-nix
            , nur
            , home-manager
            , retiolum
            , flake-utils
            , krops
            , flake-registry
            , bme680-mqtt
            , ... }:
    (flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      nurPkgs = import nur {
        inherit pkgs;
        nurpkgs = pkgs;
      };
     in {
       devShell = pkgs.callPackage ./shell.nix {};
       # deploy like this:
       #  nix run ".#deploy.turingmachine"
       #  nix run ".#deploy.eve"
       apps.deploy = pkgs.callPackage ./nixos/krops.nix {
         inherit (krops.packages.${system}) writeCommand;
         lib = krops.lib;
       };
       apps.irc-announce = nurPkgs.repos.mic92.irc-announce;
     })) // {


     nixosConfigurations = import ./nixos/configurations.nix {
       #nixpkgs = toString <nixpkgs>;
       # for testing
       #nixosSystem = import <nixpkgs/nixos/lib/eval-config.nix>;
       inherit nixpkgs nixpkgs-systemd;
       nixosSystem = nixpkgs.lib.nixosSystem;
       inherit nur home-manager sops-nix retiolum nixos-hardware flake-registry bme680-mqtt;
     };


     hmConfigurations = import ./nixpkgs-config/homes.nix {
       inherit self nixpkgs home-manager nur;
     };

     hydraJobs = {
       configurations =
         nixpkgs.lib.mapAttrs'
           (name: config: nixpkgs.lib.nameValuePair name config.config.system.build.toplevel)
           self.nixosConfigurations;
       hmConfigurations = nixpkgs.lib.mapAttrs'
         (name: config: nixpkgs.lib.nameValuePair name config.activation-script)
         self.hmConfigurations;
     };
   };
}
