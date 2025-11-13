{
  python3,
  pueue,
}:
python3.pkgs.buildPythonApplication {
  pname = "pexpect-cli";
  version = "0.1.0";
  pyproject = true;

  src = ./.;

  build-system = with python3.pkgs; [
    setuptools
    wheel
  ];

  dependencies = with python3.pkgs; [ pexpect ];

  nativeCheckInputs = with python3.pkgs; [
    pytestCheckHook
    pytest-timeout
    pueue
  ];

  # Add pueue to PATH for both runtime and tests
  makeWrapperArgs = [ "--prefix PATH : ${pueue}/bin" ];

  # Patch shebangs in bin/ directory for tests
  postPatch = ''
    patchShebangs bin/
  '';

  # Ensure binaries are available during tests
  preCheck = ''
    export PATH=$out/bin:${pueue}/bin:$PATH
  '';

  # Basic smoke test
  pythonImportsCheck = [ "pexpect_cli" ];

  meta = {
    description = "Persistent pexpect sessions via pueue";
    mainProgram = "pexpect-cli";
  };
}
