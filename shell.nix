{
  perSystem = {
    inputs',
    pkgs,
    ...
  }: {
    # Definitions like this are entirely equivalent to the ones
    # you may have directly in flake.nix.
    devShells.default = pkgs.mkShellNoCC {
      sopsPGPKeyDirs = ["./nixos/secrets/keys"];
      sopsCreateGPGHome = true;
      nativeBuildInputs = [
        inputs'.sops-nix.packages.sops-import-keys-hook
        pkgs.python3.pkgs.invoke
        pkgs.python3.pkgs.deploykit
      ];
    };
  };
}
