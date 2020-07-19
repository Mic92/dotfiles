with import <nixpkgs> {};
mkShell {
  sopsPGPKeys = [
    "../../secrets/keys/users/mic92.asc"
    "../../secrets/keys/hosts/eddie.asc"
  ];
  nativeBuildInputs = [
    (pkgs.callPackage /home/joerg/git/sops-nix {}).sops-pgp-hook
  ];
}
