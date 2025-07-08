{ lib
, python
, fetchPypi
}:

python.pkgs.buildPythonPackage rec {
  pname = "types-icalendar";
  version = "6.3.1.20250521";

  src = fetchPypi {
    pname = "types_icalendar";
    inherit version;
    hash = "sha256-yP3H5q/TmXMZG7Z87FpZGMG/TN3mSujETLwM+k4xoe8=";
  };

  pyproject = true;

  build-system = with python.pkgs; [
    setuptools
  ];

  dependencies = with python.pkgs; [
    types-pytz
    types-python-dateutil
  ];

  # This is a type stubs package, it has no tests to run
  doCheck = false;

  pythonImportsCheck = [
    "icalendar-stubs"
  ];

  meta = with lib; {
    description = "Typing stubs for icalendar";
    homepage = "https://github.com/python/typeshed";
    license = licenses.asl20;
  };
}