{
  perSystem = { inputs', pkgs, ... }: {
    devShells.terraform = pkgs.mkShell {
      buildInputs = [
        pkgs.sops
        (pkgs.terraform.withPlugins (
          p: [
            p.gitlab
            p.github
            p.sops
            p.null
            p.dns
            p.digitalocean
          ]
        ))
      ];
    };
  };
}
