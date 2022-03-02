with import <nixpkgs> {};
  mkShell {
    nativeBuildInputs = [
      bashInteractive
      flyctl
      skopeo
    ];
  }
