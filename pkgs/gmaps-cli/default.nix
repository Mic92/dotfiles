{
  lib,
  buildPythonApplication,
  setuptools,
}:

buildPythonApplication {
  pname = "gmaps-cli";
  version = "0.1.0";

  src = ./.;

  pyproject = true;

  build-system = [ setuptools ];

  dependencies = [ ];

  meta = {
    description = "CLI tool to search for places using Google Maps API";
    mainProgram = "gmaps-cli";
    license = lib.licenses.mit;
    maintainers = [ ];
  };
}
