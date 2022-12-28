with import <nixpkgs> { };
mkShell {
  nativeBuildInputs = [
    bashInteractive

    (pkgs.terraform.withPlugins (
      p: [
        p.vault
        p.consul
      ]
    ))
    consul
    vault
  ];
}
