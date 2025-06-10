{
  lib,
  python3,
}:

python3.pkgs.buildPythonApplication {
  pname = "tmux-mcp";
  version = "0.1.0";
  pyproject = true;

  src = ./.;

  build-system = with python3.pkgs; [
    setuptools
    wheel
  ];

  dependencies = with python3.pkgs; [
    mcp
    pydantic
    anyio
  ];

  nativeCheckInputs = with python3.pkgs; [
    pytest
    pytest-asyncio
  ];

  # Enable tests
  doCheck = true;

  # Test configuration
  pytestFlagsArray = [ "tests/" ];

  meta = with lib; {
    description = "Model Context Protocol server for tmux integration";
    homepage = "https://github.com/Mic92/dotfiles";
    license = licenses.mit;
    maintainers = [ ];
    mainProgram = "tmux-mcp";
  };
}
