{
  lib,
  buildPythonPackage,
  fetchFromGitea,
  poetry-core,
  pythonRelaxDepsHook,
  matrix-nio,
  cryptography,
  pillow,
  markdown,
  toml,
  python-cryptography-fernet-wrapper,
}:

buildPythonPackage rec {
  pname = "simplematrixbotlib";
  version = "unstable-2025-02-09";
  pyproject = true;

  src = fetchFromGitea {
    domain = "codeberg.org";
    owner = "imbev";
    repo = "simplematrixbotlib";
    rev = "a4e649091b68ea48d7461813fd349b2c6f99b8f4";
    hash = "sha256-RwMRcSKtSz/UQKZI//utgE/iAv5g4zel1eHlsRk9uZk=";
  };

  build-system = [ poetry-core ];

  nativeBuildInputs = [ pythonRelaxDepsHook ];

  pythonRelaxDeps = [ "pillow" ];

  dependencies = [
    matrix-nio
    cryptography
    pillow
    markdown
    toml
    python-cryptography-fernet-wrapper
  ];

  # Tests require a Matrix homeserver
  doCheck = false;

  pythonImportsCheck = [ "simplematrixbotlib" ];

  meta = {
    description = "Simple Matrix bot library";
    homepage = "https://codeberg.org/imbev/simplematrixbotlib";
    license = lib.licenses.mit;
    maintainers = [ ];
  };
}
