{
  python3,
  pueue,
  lldb,
  rr,
  lib,
  stdenv,
}:
python3.pkgs.buildPythonApplication {
  pname = "debugger-cli";
  version = "0.1.0";
  pyproject = true;

  src = ./.;

  build-system = with python3.pkgs; [
    setuptools
    wheel
  ];

  # No Python dependencies - uses system lldb bindings
  dependencies = [ ];

  nativeCheckInputs = with python3.pkgs; [
    pytestCheckHook
    pytest-timeout
    pueue
  ];

  # Add pueue, lldb, and rr to PATH for runtime
  makeWrapperArgs = [
    "--prefix PATH : ${lib.makeBinPath [ pueue lldb rr ]}"
    # LLDB Python bindings path
    "--prefix PYTHONPATH : ${lldb}/lib/python${python3.pythonVersion}/site-packages"
  ];

  # Patch shebangs in bin/ directory for tests
  postPatch = ''
    patchShebangs bin/
  '';

  # Ensure binaries are available during tests
  preCheck = ''
    export PATH=$out/bin:${pueue}/bin:$PATH
  '';

  # Basic smoke test
  pythonImportsCheck = [ "debugger_cli" ];

  meta = {
    description = "LLM-optimized debugger interface for LLDB and RR";
    mainProgram = "debugger-cli";
    platforms = lib.platforms.unix;
  };
}
