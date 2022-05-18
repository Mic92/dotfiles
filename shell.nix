{
  perSystem = system: { inputs', pkgs, ... }: {
    # Definitions like this are entirely equivalent to the ones
    # you may have directly in flake.nix.
    devShells.default = pkgs.mkShell {
      sopsPGPKeyDirs = ["./nixos/secrets/keys"];
      sopsCreateGPGHome = true;
      nativeBuildInputs = [
        inputs'.sops-nix.packages.sops-import-keys-hook
        pkgs.python3.pkgs.invoke
      ];
    };
  };
}
