let
  flake = builtins.getFlake (toString ../../../..);
in
with import flake.inputs.nixpkgs { };
mkShell {
  nativeBuildInputs = [
    bashInteractive
    sops
    gnupg
    (terraform.withPlugins (p: [ p.hydra ]))
  ];
}
