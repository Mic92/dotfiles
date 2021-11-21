with import <nixpkgs> {};
mkShell {
  nativeBuildInputs = [
    bashInteractive
    sops
    gnupg
    (pkgs.terraform.withPlugins (
      p: [
        p.gitlab
        p.github
        p.sops
      ]
    ))
  ];
}
