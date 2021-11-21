with import <nixpkgs> {};
mkShell {
  nativeBuildInputs = [
    bashInteractive
    (pkgs.terraform.withPlugins (
      p: [
        p.gitlab
        p.sops
      ]
    ))
  ];
}
