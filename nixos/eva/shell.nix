let
  flake = builtins.getFlake (toString ./../..);
in
with import flake.inputs.nixpkgs { };
mkShell {
  nativeBuildInputs = [
    bashInteractive
    (terraform.withPlugins (p: [ p.aws ]))
    awscli
  ];
}
