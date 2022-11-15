{ buildGoModule, lib, fetchFromGitLab, terraform-providers }:

let
  owner = "gitlab-org";
  repo = "terraform-provider-gitlab";
  version = "3.19.0";
  rev = "v${version}";
  hash = "sha256-1Ljf9kwpj96mzu/uHqitYCKIixNn/sZL21zOM8xQsU4=";
in
(terraform-providers.mkProvider rec {
  inherit owner repo version rev hash;
  vendorHash = "sha256-e9J4g5ZuiKcI/WSXMFY3Qglgt87qbXv7tDpxYbRRuaU=";
  provider-source-address = "registry.terraform.io/gitlabhq/gitlab";
}).overrideAttrs (old: {
  src = fetchFromGitLab {
    name = "source-${rev}";
    inherit owner repo rev hash;
  };

  meta = with lib; {
    description = "Official GitLab Terraform Provider";
    homepage = "https://registry.terraform.io/providers/gitlabhq/gitlab";
    license = licenses.mpl20;
    maintainers = with maintainers; [ mic92 ];
    platforms = platforms.all;
  };
})
