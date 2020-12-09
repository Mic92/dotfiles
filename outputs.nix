{ self
, nixpkgs
, nixos-hardware
, sops-nix
, nur
, home-manager
, retiolum
, flake-utils
, krops
, flake-registry
, bme680-mqtt
, nix-ld
, doom-emacs
, nix-doom-emacs
}:
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
    inherit nixpkgs;
    nixosSystem = nixpkgs.lib.nixosSystem;
    inherit
      nur
      home-manager
      sops-nix
      retiolum
      nixos-hardware
      flake-registry bme680-mqtt
      nix-ld;
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
}
