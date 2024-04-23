{
  buildPythonPackage,
  ruamel-yaml,
  setuptools,
}:
buildPythonPackage {
  pname = "mergify-gen-config";
  version = "1.0.0";
  format = "pyproject";
  src = ./.;
  nativeBuildInputs = [ setuptools ];
  propagatedBuildInputs = [ ruamel-yaml ];
}
