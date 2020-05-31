{ lib
, buildPythonPackage
, fetchPypi
, aiohttp
}:

buildPythonPackage rec {
  pname = "coronavirus";
  version = "1.1.0";

  src = fetchPypi {
    inherit pname version;
    sha256 = "38f47bed399967329d06a784081b427e94a5055ad7487875fb8af8b58d4db611";
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
