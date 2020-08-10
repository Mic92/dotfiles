with import <nixpkgs> {};
let
  flake = builtins.getFlake (toString ./../..);
in
mkShell {
  sopsPGPKeyDirs = [
    "./keys/hosts"
    "./keys/users"
  ];
  nativeBuildInputs = [
    (pkgs.callPackage flake.inputs.sops-nix {}).sops-pgp-hook
  ];
}
