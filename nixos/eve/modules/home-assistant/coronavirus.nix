{ lib
, buildPythonPackage
, fetchPypi
, aiohttp
}:

buildPythonPackage rec {
  pname = "coronavirus";
  version = "1.1.1";

  src = fetchPypi {
    inherit pname version;
    sha256 = "1xpckp5zyspbgia2lkk9f9fxkkr9lam9wl3lx9fwffiid7lgkbgw";
  };

  propagatedBuildInputs = [
    aiohttp
  ];

  meta = with lib; {
    description = "Asynchronous Python client for getting Corona virus info";
    homepage = https://github.com/nabucasa/coronavirus;
    license = licenses.mit;
  };
}
