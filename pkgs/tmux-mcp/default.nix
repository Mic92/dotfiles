{
  lib,
  python3,
  ripgrep,
  makeWrapper,
}:

python3.pkgs.buildPythonApplication {
  pname = "tmux-mcp";
  version = "0.1.0";
  pyproject = true;

  src = ./.;

  nativeBuildInputs = [ makeWrapper ];

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
    ripgrep
  ];

  # Enable tests
  doCheck = true;

  # Test configuration
  pytestFlagsArray = [ "tests/" ];

  # Wrap the binary to include ripgrep in PATH
  postInstall = ''
    wrapProgram $out/bin/tmux-mcp \
      --prefix PATH : ${lib.makeBinPath [ ripgrep ]}
  '';

  meta = with lib; {
    description = "Model Context Protocol server for tmux integration";
    homepage = "https://github.com/Mic92/dotfiles";
    license = licenses.mit;
    maintainers = [ ];
    mainProgram = "tmux-mcp";
  };
}
