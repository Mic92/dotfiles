{
  perSystem = {pkgs, ...}: {
    devShells.terraform = pkgs.mkShell {
      buildInputs = [
        pkgs.sops
        pkgs.terragrunt
        (pkgs.terraform.withPlugins (
          p: [
            p.gitlab
            p.github
            p.sops
            p.null
            p.dns
            p.digitalocean
            (pkgs.callPackage ./gitlab.nix {})
          ]
        ))
      ];
    };
  };
}
