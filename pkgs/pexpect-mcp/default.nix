{
  buildPythonPackage,
  setuptools,
  wheel,
  pexpect,
  mcp,
  pygments,
}:

buildPythonPackage {
  pname = "pexpect-mcp";
  version = "0.1.0";

  src = ./.;

  pyproject = true;

  nativeBuildInputs = [
    setuptools
    wheel
  ];

  propagatedBuildInputs = [
    pexpect
    mcp
    pygments
  ];

  pythonImportsCheck = [ "pexpect_mcp" ];

  meta = {
    description = "MCP server for pexpect - allows running Python code with pexpect library access";
    mainProgram = "pexpect-mcp";
  };
}
