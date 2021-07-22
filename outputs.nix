{ self
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
, nix-ld
, envfs
, doom-emacs
, nix-doom-emacs
, lambda-pirate
, nixpkgs-stable
, hercules-ci
, nix-darwin
, vmsh
, fenix
}:
(flake-utils.lib.eachDefaultSystem (system:
  let
    pkgs = nixpkgs.legacyPackages.${system};
    nurPkgs = import nur {
      inherit pkgs;
      nurpkgs = pkgs;
    };
    pkgsStable = nixpkgs-stable.legacyPackages.${system};
  in
  {
    devShell = pkgs.callPackage ./shell.nix {
      inherit (sops-nix.packages.${pkgs.system}) sops-import-keys-hook;
    };
    # deploy like this:
    #  nix run ".#deploy.turingmachine"
    #  nix run ".#deploy.eve"
    apps.deploy = pkgs.callPackage ./nixos/krops.nix {
      inherit (krops.packages.${pkgs.system}) writeCommand;
      lib = krops.lib;
    };
    apps.irc-announce = {
      type = "app";
      path = "${nurPkgs.repos.mic92.irc-announce}/bin/irc-announce";
    };

    apps.hm-build = {
      type = "app";
      program = toString (pkgs.writeScript "hm-build" ''
        #!${pkgs.runtimeShell}
        set -eu -o pipefail -x
        export PATH=${pkgs.lib.makeBinPath [ pkgs.git pkgs.coreutils pkgs.nixFlakes pkgs.jq ]}
        declare -A profiles=(["turingmachine"]="desktop" ["eddie"]="desktop" ["eve"]="eve" ["bernie"]="bernie", ["grandalf"]="common-aarch64")
        profile=''${profiles[$HOSTNAME]:-common}
        flake=$(nix flake metadata --json ${./.} | jq -r .url)
        nix build --show-trace --json ".#hmConfigurations.''${profile}.activationPackage" "$@" | jq -r '.[] | .outputs | .out'
      '');
    };
    apps.hm-switch = {
      type = "app";
      program = toString (pkgs.writeScript "hm-switch" ''
        #!${pkgs.runtimeShell}
        set -eu -o pipefail -x
        cd ${./.}
        $(nix run .#hm-build -- "$@")/activate
      '');
    };
  })) // {
  nixosConfigurations = import ./nixos/configurations.nix {
    nixosSystem = nixpkgs.lib.nixosSystem;
    inherit
      nur
      nixpkgs
      home-manager
      sops-nix
      retiolum
      nixos-hardware
      flake-registry
      bme680-mqtt
      envfs
      nix-ld
      nixpkgs-systemd
      nixpkgs-stable
      lambda-pirate
      vmsh
      hercules-ci;
  };

  hmConfigurations = import ./nixpkgs-config/homes.nix {
    inherit self nixpkgs home-manager nur nix-doom-emacs;
  };

  hydraJobs = (nixpkgs.lib.mapAttrs' (name: config: nixpkgs.lib.nameValuePair "nixos-${name}" config.config.system.build.toplevel) self.nixosConfigurations)
              // (nixpkgs.lib.mapAttrs' (name: config: nixpkgs.lib.nameValuePair "home-manager-${name}" config.activation-script) self.hmConfigurations);
}
