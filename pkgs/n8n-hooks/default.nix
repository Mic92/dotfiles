{
  lib,
  python3Packages,
}:

python3Packages.buildPythonApplication {
  pname = "n8n-hooks";
  version = "0.1.0";

  src = ./.;

  pyproject = true;

  build-system = [ python3Packages.hatchling ];

  # stdlib-only — no runtime dependencies

  nativeCheckInputs = [ python3Packages.pytest ];

  checkPhase = ''
    runHook preCheck
    pytest tests/
    runHook postCheck
  '';

  meta = {
    description = "CLI to invoke n8n webhooks — currently: store email drafts in IMAP";
    mainProgram = "n8n-hooks";
    license = lib.licenses.mit;
  };
}
