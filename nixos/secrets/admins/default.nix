with import <nixpkgs> { };
mkShell {
  nativeBuildInputs = [
    bashInteractive
    cfssl
    sops
  ];
}
