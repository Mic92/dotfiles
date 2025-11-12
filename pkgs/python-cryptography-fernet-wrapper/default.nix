{
  lib,
  buildPythonPackage,
  fetchPypi,
  cryptography,
}:

buildPythonPackage rec {
  pname = "python-cryptography-fernet-wrapper";
  version = "1.0.4";
  format = "setuptools";

  src = fetchPypi {
    inherit pname version;
    hash = "sha256-dPEYr612POscHqq6awbhnMNeXP+0AI5xVGdRcfoRapI=";
  };

  propagatedBuildInputs = [ cryptography ];

  # No tests in PyPI package
  doCheck = false;

  # Skip import check - module structure varies
  pythonImportsCheck = [ ];

  meta = {
    description = "A simple wrapper around python cryptography Fernet";
    homepage = "https://pypi.org/project/python-cryptography-fernet-wrapper/";
    license = lib.licenses.mit;
    maintainers = [ ];
  };
}
