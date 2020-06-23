
{
  description = "A flake for building Hello World";

  inputs.nixpkgs.url = github:Mic92/nixpkgs/master;

  outputs = { self, nixpkgs }: {

    packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.hello;

    nixosConfiguration.turingmachine = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./nixos/turingmachine/configuration.nix
        { system.configurationRevision = self.rev;
          /* typical configuration.nix stuff follows */
        }
      ];
    };
  };
}
