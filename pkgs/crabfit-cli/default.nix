{
  buildPythonApplication,
  hatchling,
}:

buildPythonApplication {
  pname = "crabfit-cli";
  version = "0.1.0";
  src = ./.;
  pyproject = true;

  build-system = [ hatchling ];

  meta = {
    description = "CLI for creating and managing Crab.fit scheduling events";
    mainProgram = "crabfit-cli";
  };
}
