{self, ...}: {
  perSystem = {
    inputs',
    pkgs,
    self',
    ...
  }: let
    inherit (self.inputs) nixos-generators nur nixpkgs;

    containerPkgs = inputs'.nix2container.packages;

    copyToPodman = image:
      pkgs.writeShellScriptBin "copy-to-podman" ''
        ${containerPkgs.skopeo-nix2container}/bin/skopeo --insecure-policy copy nix:${image} containers-storage:${image.name}:${image.tag}
        echo Docker image ${image.name}:${image.tag} have been loaded
      '';

    defaultModule = { config, ... }: {
      imports = [
        ./base-config.nix
        self.inputs.nur.nixosModules.nur
      ];
      _module.args.inputs = self.inputs;
      system.stateVersion = config.system.nixos.version;
    };
  in {
    packages = {
      # nix build '.#kexec'
      kexec = nixos-generators.nixosGenerate {
        inherit pkgs;
        modules = [
          defaultModule
          ./kexec.nix
        ];
        format = "kexec";
      };

      kexec-aarch64 = nixos-generators.nixosGenerate {
        pkgs = nixpkgs.legacyPackages.aarch64-linux;
        modules = [
          defaultModule
          ./kexec.nix
        ];
        format = "kexec";
      };

      sd-image-aarch64 = nixos-generators.nixosGenerate {
        pkgs = nixpkgs.legacyPackages.aarch64-linux;
        modules = [
          defaultModule
        ];
        format = "install-iso";
      };

      sd-image = nixos-generators.nixosGenerate {
        inherit pkgs;
        modules = [
          defaultModule
        ];
        format = "install-iso";
      };

      netboot = pkgs.callPackage ./netboot.nix {
        inherit pkgs;
        inherit (nixpkgs.lib) nixosSystem;
        extraModules = [
          defaultModule
        ];
      };

      netboot-pixie-core = pkgs.callPackage ./netboot-pixie-core.nix {
        inherit (self'.packages) netboot;
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
