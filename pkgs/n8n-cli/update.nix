{
  writeShellApplication,
  python3,
  gh,
  nix,
  bun,
  bun2nix,
}:
writeShellApplication {
  name = "update-n8n-cli";
  runtimeInputs = [
    (python3.withPackages (_ps: [ ]))
    gh
    nix
    bun
    bun2nix
  ];
  text = ''
    exec python3 ${./update.py}
  '';
}
