with import <nixpkgs> {};
mkShell {
  sopsPGPKeyDirs = [
    "./keys/hosts"
    "./keys/users"
  ];
  nativeBuildInputs = [
    (pkgs.callPackage /home/joerg/git/sops-nix {}).sops-pgp-hook
  ];
}
