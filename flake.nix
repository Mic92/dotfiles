{
  description = "NixOS configuration with flakes";

  # To update all inputs:
  # $ nix flake update --recreate-lock-file
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    nixpkgs.url = "github:Mic92/nixpkgs/master";
    # for development
    #nixpkgs.url = "/home/joerg/git/nixpkgs";
    nur.url = "github:nix-community/NUR";
    sops-nix.url = "github:Mic92/sops-nix";

    krops.url = "github:krebs/krops";
    krops.flake = false;
    retiolum.url = "git+https://git.thalheim.io/Mic92/retiolum";
    # for development
    #sops-nix.url = "/home/joerg/git/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    nixos-hardware.url = "github:Mic92/nixos-hardware/master";
    home-manager.url = "github:rycee/home-manager";
    home-manager.flake = false;

    choose-place.url = "github:mbailleu/choose-place";
    choose-place.flake = false;

    doom-emacs.url = "github:hlissner/doom-emacs";
    doom-emacs.flake = false;

    nix-doom-emacs.url = "github:vlaci/nix-doom-emacs";
    nix-doom-emacs.flake = false;
  };

  outputs = { self
            , nixpkgs
            , nixos-hardware
            , sops-nix
            , nur
            , home-manager
            , choose-place
            , retiolum
            , flake-utils
            , ... }:
    (flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
     in {
       devShell = pkgs.mkShell {
         # is this a good idea?
         #nativeBuildInputs = [ pkgs.ansible ];
       };
     })) // {
     nixosConfigurations = import ./nixos/configurations.nix {
       inherit nixpkgs nur home-manager sops-nix retiolum nixos-hardware choose-place;
     };

     hmConfigurations = import ./nixpkgs-config/homes.nix {
       inherit self nixpkgs home-manager;
     };

     inherit (nixpkgs) legacyPackages;

     hydraJobs = {
       configurations = nixpkgs.lib.mapAttrs' (name: config:
         nixpkgs.lib.nameValuePair name config.config.system.build.toplevel)
         self.nixosConfigurations;
       hmConfigurations = nixpkgs.lib.mapAttrs' (name: config:
         nixpkgs.lib.nameValuePair name config.activate)
         self.hmConfigurations;
     };
   };
}
