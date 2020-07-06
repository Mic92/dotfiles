
{
  description = "NixOS configuration with flakes";

  inputs.nixpkgs.url = github:Mic92/nixpkgs/master;
  inputs.sops-nix.url = "/home/joerg/git/sops-nix";
  inputs.sops-nix.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nixos-hardware.url = github:Mic92/nixos-hardware/master;

  outputs = { self, nixpkgs, nixos-hardware, sops-nix }: {
    nixosConfigurations.turingmachine = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        { system.configurationRevision = self.rev; }
        nixos-hardware.nixosModules.dell.xps-13-9380
        sops-nix.nixosModules.sops
        ./nixos/turingmachine/configuration.nix
      ];
    };
  };
}
