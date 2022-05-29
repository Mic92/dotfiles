{
  perSystem = { inputs', pkgs, ... }: {
    devShells.terranix = pkgs.mkShell {
      buildInputs = [
        (pkgs.terraform.withPlugins (
          p: [
            p.gitlab
            p.github
            p.sops
            p.null
          ]
        ))
      ];
    };
  };
}
