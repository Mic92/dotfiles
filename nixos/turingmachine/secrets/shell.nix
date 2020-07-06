with import <nixpkgs> {};
mkShell {
  sopsGPGKeys = [
    "../../secrets-sops/keys/users/mic92.asc"
    "../../secrets-sops/keys/hosts/turingmachine.asc"
  ];
  nativeBuildInputs = [
    (pkgs.callPackage /home/joerg/git/sops-nix {}).sops-shell-hook
  ];
}
