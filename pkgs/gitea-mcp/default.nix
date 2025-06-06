{
  lib,
  buildGoModule,
  fetchFromGitea,
}:

buildGoModule rec {
  pname = "gitea-mcp";
  version = "0.1.0";

  src = fetchFromGitea {
    domain = "gitea.com";
    owner = "gitea";
    repo = "gitea-mcp";
    rev = "main";
    hash = "sha256-vokTQHEHm0UZE4fa7mGXRMEdX4Gi50VwHAQ3enXDhlo=";
  };

  vendorHash = "sha256-u9jIjrbDUhnaaeBET+pKQTKhaQLUeQvKOXSBfS0vMJM=";

  ldflags = [
    "-s"
    "-w"
  ];

  meta = with lib; {
    description = "Model Context Protocol server for Gitea";
    homepage = "https://gitea.com/gitea/gitea-mcp";
    license = licenses.mit;
    maintainers = [ ];
    mainProgram = "gitea-mcp";
  };
}
