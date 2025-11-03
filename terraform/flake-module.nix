{
  perSystem =
    { pkgs, ... }:
    {
      devShells.terraform = pkgs.mkShell {
        buildInputs = [
          pkgs.sops
          pkgs.terragrunt
          (pkgs.opentofu.withPlugins (p: [
            p.gitlabhq_gitlab
            p.integrations_github
            p.carlpett_sops
            p.hashicorp_null
            p.hashicorp_dns
            p.digitalocean_digitalocean
          ]))
        ];
      };
    };
}
