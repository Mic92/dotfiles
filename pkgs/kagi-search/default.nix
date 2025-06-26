{
  lib,
  buildPythonApplication,
  setuptools,
  beautifulsoup4,
}:

buildPythonApplication {
  pname = "kagi-search";
  version = "0.1.0";

  src = ./.;

  pyproject = true;

  build-system = [ setuptools ];

  dependencies = [ beautifulsoup4 ];

  meta = {
    description = "CLI tool for searching Kagi using session tokens";
    mainProgram = "kagi-search";
    license = lib.licenses.mit;
    maintainers = [ ];
  };
}
