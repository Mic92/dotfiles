{
  lib,
  buildPythonApplication,
  hatchling,
  simplematrixbotlib,
  aiohttp,
}:

buildPythonApplication {
  pname = "calendar-bot";
  version = "0.1.0";

  src = ./.;

  pyproject = true;

  build-system = [ hatchling ];

  dependencies = [
    simplematrixbotlib
    aiohttp
  ];

  meta = {
    description = "Matrix calendar bot bridge for n8n";
    mainProgram = "calendar-bot";
    license = lib.licenses.mit;
    maintainers = [ ];
  };
}
