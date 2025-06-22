{
  lib,
  python3,
}:

python3.pkgs.buildPythonApplication {
  pname = "paperless-cli";
  version = "0.1.0";

  src = ./.;

  format = "pyproject";

  nativeBuildInputs = with python3.pkgs; [
    setuptools
    wheel
  ];

  pythonImportsCheck = [ "paperless_cli" ];

  meta = with lib; {
    description = "CLI tool for managing Paperless-ngx documents, mail accounts, and rules";
    license = licenses.mit;
    maintainers = [ ];
    mainProgram = "paperless-cli";
  };
}
