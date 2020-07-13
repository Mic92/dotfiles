
{
  description = "NixOS configuration with flakes";

  # To update all inputs:
  # nix flake list-inputs --json | jq -r '.nodes | keys | .[]' | xargs -n1 nix flake update --update-input
  inputs.nixpkgs.url = github:Mic92/nixpkgs/master;
  inputs.nur.url = github:nix-community/NUR;
  inputs.sops-nix.url = github:Mic92/sops-nix;
  # for development
  #inputs.sops-nix.url = "/home/joerg/git/sops-nix";
  inputs.sops-nix.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nixos-hardware.url = github:Mic92/nixos-hardware/master;
  inputs.home-manager.url = github:rycee/home-manager;
  inputs.home-manager.flake = false;

  outputs = { self, nixpkgs, nixos-hardware, sops-nix, nur, home-manager }: {
    nixosConfigurations.turingmachine = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        { nixpkgs.overlays = [ nur.overlay ]; }
        nixos-hardware.nixosModules.dell-xps-13-9380
        sops-nix.nixosModules.sops
        ./nixos/turingmachine/configuration.nix
      ];
    };
    nixosConfigurations.eddie = nixpkgs.lib.nixosSystem { 
      system = "x86_64-linux";
      modules = [
        { nixpkgs.overlays = [ nur.overlay ]; }
        sops-nix.nixosModules.sops
        ./nixos/eddie/configuration.nix
      ];
    };

    hmConfigurations.joerg = let
      pkgs = nixpkgs.legacyPackages."x86_64-linux";
    in rec {
      generation = import "${home-manager.outPath}/home-manager/home-manager.nix" {
        confPath = "${self}/nixpkgs-config/common.nix";
        inherit pkgs;
      };
      activate = pkgs.writeShellScript "home-manager-activate" ''
        #!${pkgs.runtimeShell}
        exec ${generation.activationPackage}/activate
      '';
    };
  };
}
