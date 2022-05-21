{self, ...}: {
  perSystem = system: {
    config,
    self',
    inputs',
    pkgs,
    ...
  }: let
    inherit (self.inputs) nixos-generators nur nixpkgs;

    containerPkgs = inputs'.nix2container.packages;
    selfPkgs = self.packages.${system};

    copyToPodman = image:
      pkgs.writeShellScriptBin "copy-to-podman" ''
        ${containerPkgs.skopeo-nix2container}/bin/skopeo --insecure-policy copy nix:${image} containers-storage:${image.name}:${image.tag}
        echo Docker image ${image.name}:${image.tag} have been loaded
      '';
    nur-overlay = {nixpkgs.overlays = [nur.overlay];};
    inputs-module = {
      _module.args.inputs = self.inputs;
    };
  in {
    packages = {
      # nix build '.#kexec' --impure
      #kexec = nixos-generators.nixosGenerate {
      #  inherit pkgs;
      #  modules = [
      #    ./kexec.nix
      #    nur-overlay
      #    inputs-module
      #  ];
      #  format = "kexec";
      #};

      #kexec-aarch64 = nixos-generators.nixosGenerate {
      #  pkgs = nixpkgs.legacyPackages.aarch64-linux;
      #  modules = [
      #    ./kexec.nix
      #    inputs-module
      #  ];
      #  format = "kexec";
      #};

      sd-image = nixos-generators.nixosGenerate {
        inherit pkgs;
        modules = [
          ./base-config.nix
          nur-overlay
          inputs-module
          ({config, ...}: {system.stateVersion = config.system.nixos.version;})
        ];
        format = "install-iso";
      };

      netboot = pkgs.callPackage ./netboot.nix {
        inherit pkgs;
        inherit (nixpkgs.lib) nixosSystem;
        extraModules = [
          inputs-module
          ({config, ...}: {system.stateVersion = config.system.nixos.version;})
        ];
      };

      netboot-pixie-core = pkgs.callPackage ./netboot-pixie-core.nix {
        inherit (selfPkgs) netboot;
      };

      nspawn-template = import ./nspawn-template.nix {
        inherit nixos-generators;
        inherit pkgs;
      };

      #kresd-image = pkgs.callPackage ./kresd.nix {
      #  inherit (containerPkgs) nix2container;
      #};
      #kresd-podman = copyToPodman selfPkgs.kresd-image;
      #kresd-registry = selfPkgs.kresd-image.copyToRegistry;

      #headscale-podman = copyToPodman selfPkgs.headscale-image;
      #headscale-image = pkgs.callPackage ./nixos/images/headscale.nix {
      #  inherit (containerPkgs) nix2container;
      #};
    };
  };
}
