
{
  description = "NixOS configuration with flakes";

  inputs.nixpkgs.url = github:Mic92/nixpkgs/master;
  inputs.nur.url = github:nix-community/NUR;
  inputs.sops-nix.url = github:Mic92/sops-nix;
  # for development
  #inputs.sops-nix.url = "/home/joerg/git/sops-nix";
  inputs.sops-nix.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nixos-hardware.url = github:Mic92/nixos-hardware/master;

  outputs = { self, nixpkgs, nixos-hardware, sops-nix, nur }: {
    nixosConfigurations.turingmachine = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        { nixpkgs.overlays = [ nur.overlay ]; }
        nixos-hardware.nixosModules.dell.xps-13-9380
        sops-nix.nixosModules.sops
        ./nixos/turingmachine/configuration.nix
      ];
    };
  };
}
