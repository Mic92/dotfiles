{
  buildPythonApplication,
  hatchling,
}:

buildPythonApplication {
  pname = "claude-md";
  version = "0.1.0";
  src = ./.;
  pyproject = true;

  build-system = [ hatchling ];

  meta = {
    description = "CLI tool to manage CLAUDE.local.md files across repositories";
    mainProgram = "claude-md";
  };
}
