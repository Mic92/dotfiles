let
  flake = builtins.getFlake (toString ./../..);
in
with import flake.inputs.nixpkgs {};
mkShell {
  sopsPGPKeys = [
    "../secrets/keys/users/mic92.asc"
  ];

  nativeBuildInputs = [
    bashInteractive
    (terraform.withPlugins (p: [ p.aws ]))
    awscli
    (pkgs.callPackage flake.inputs.sops-nix {}).sops-pgp-hook
  ];
}
