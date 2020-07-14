with import <nixpkgs> {};
mkShell {
  sopsPGPKeys = [
    "../../secrets-sops/keys/users/mic92.asc"
    "../../secrets-sops/keys/hosts/eddie.asc"
  ];
  nativeBuildInputs = [
    (pkgs.callPackage /home/joerg/git/sops-nix {}).sops-pgp-hook
  ];
}
