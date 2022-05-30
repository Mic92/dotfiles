{
  perSystem = { inputs', pkgs, ... }: {
    devShells.terraform = pkgs.mkShell {
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
