{
  buildPythonApplication,
  setuptools,
}:

buildPythonApplication {
  pname = "claude-md";
  version = "0.1.0";
  src = ./.;
  pyproject = true;

  build-system = [ setuptools ];

  meta = {
    description = "CLI tool to manage CLAUDE.local.md files across repositories";
    mainProgram = "claude-md";
  };
}
