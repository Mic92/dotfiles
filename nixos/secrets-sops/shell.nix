with import <nixpkgs> {};
mkShell {
  sopsGPGKeyDirs = [
    "./keys/hosts"
    "./keys/users"
  ];
  nativeBuildInputs = [
    (pkgs.callPackage /home/joerg/git/sops-nix {}).sops-shell-hook
  ];
}
