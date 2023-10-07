with import <nixpkgs> {};
mkShell {
  packages = [
    bashInteractive
    qtile
    mypy
  ];
}
