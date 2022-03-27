{
  self,
  flake-utils,
  nixpkgs,
  nur,
  sops-nix,
  nixos-generators,
  nix2container,
  ...
} @ inputs:
(flake-utils.lib.eachDefaultSystem (system: let
  pkgs = nixpkgs.legacyPackages.${system};
  nurPkgs = import nur {
    inherit pkgs;
    nurpkgs = pkgs;
  };
in {
  devShell = pkgs.callPackage ./shell.nix {
    inherit (sops-nix.packages.${pkgs.system}) sops-import-keys-hook;
  };

  apps.hm-build = {
    type = "app";
    program = toString (pkgs.writeScript "hm-build" ''
      #!${pkgs.runtimeShell}
      set -eu -o pipefail
      export PATH=${pkgs.lib.makeBinPath [pkgs.git pkgs.coreutils pkgs.nixFlakes pkgs.jq]}
      declare -A profiles=(["turingmachine"]="desktop" ["eddie"]="desktop" ["eve"]="eve" ["bernie"]="bernie" ["grandalf"]="common-aarch64" ["yasmin"]="common-aarch64")
      profile=common
      if [[ -n ''${profiles[$HOSTNAME]:-} ]]; then
        profile=''${profiles[$HOSTNAME]}
      fi
      flake=$(nix flake metadata --json ${self} | jq -r .url)
      nix build --no-link --show-trace --json "${self}#hmConfigurations.''${profile}.activationPackage" "$@" | jq -r '.[] | .outputs | .out'
    '');
  };
  apps.hm-switch = {
    type = "app";
    program = toString (pkgs.writeScript "hm-switch" ''
      #!${pkgs.runtimeShell}
      export PATH=${pkgs.lib.makeBinPath [pkgs.nix pkgs.coreutils]}
      set -eu -o pipefail -x
      cd ${./.}
      oldpath=$(realpath /nix/var/nix/profiles/per-user/$USER/home-manager)
      path=$(nix run .#hm-build -- "$@")
      nix store diff-closures "$oldpath" "$path"
      $path/activate
    '');
  };
}))
// {
  nixosConfigurations = import ./nixos/configurations.nix (inputs
    // {
      inherit inputs;
    });

  # nix build '.#kexec' --impure
  packages.x86_64-linux = let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
    containerPkgs = nix2container.packages.x86_64-linux;
    selfPkgs = self.packages.x86_64-linux;

    copyToPodman = image:
      pkgs.writeShellScriptBin "copy-to-podman" ''
        ${containerPkgs.skopeo-nix2container}/bin/skopeo --insecure-policy copy nix:${image} containers-storage:${image.name}:${image.tag}
        echo Docker image ${image.name}:${image.tag} have been loaded
      '';
  in {
    kexec = nixos-generators.nixosGenerate {
      inherit pkgs;
      modules = [
        ./nixos/images/kexec.nix
        {nixpkgs.overlays = [nur.overlay];}
      ];
      format = "kexec";
    };

    nspawn-template = import ./nixos/images/nspawn-template.nix {
      inherit nixos-generators;
      inherit pkgs;
    };

    kexec-aarch64 = nixos-generators.nixosGenerate {
      pkgs = nixpkgs.legacyPackages.aarch64-linux;
      modules = [
        ./nixos/images/kexec.nix
        {nixpkgs.overlays = [nur.overlay];}
      ];
      format = "kexec";
    };


    kresd-image = pkgs.callPackage ./nixos/images/kresd.nix {
      inherit (containerPkgs) nix2container;
    };

    #headscale-image = pkgs.callPackage ./nixos/images/headscale.nix {
    #  inherit (containerPkgs) nix2container;
    #};

    kresd-podman = copyToPodman selfPkgs.kresd-image;

    #headscale-podman = copyToPodman selfPkgs.headscale-image;

    kresd-registry = selfPkgs.kresd-image.copyToRegistry;

    netboot = pkgs.callPackage ./nixos/images/netboot.nix {
      inherit pkgs;
      inherit (nixpkgs.lib) nixosSystem;
      extraModules = [
        {_module.args.inputs = inputs;}
      ];
    };

    netboot-pixie-core = pkgs.callPackage ./nixos/images/netboot-pixie-core.nix {
      inherit (selfPkgs) netboot;
    };
  };

  hmConfigurations = import ./nixpkgs-config/homes.nix inputs;

  hydraJobs =
    (nixpkgs.lib.mapAttrs' (name: config: nixpkgs.lib.nameValuePair "nixos-${name}" config.config.system.build.toplevel) self.nixosConfigurations)
    // (nixpkgs.lib.mapAttrs' (name: config: nixpkgs.lib.nameValuePair "home-manager-${name}" config.activation-script) self.hmConfigurations);
}
