{
  lib,
  buildPythonApplication,
  hatchling,
  mautrix,
  aiohttp,
  asyncpg,
  python-olm,
  unpaddedbase64,
  pycryptodome,
  base58,
}:

buildPythonApplication {
  pname = "calendar-bot";
  version = "0.3.0";

  src = ./.;

  pyproject = true;

  build-system = [ hatchling ];

  dependencies = [
    mautrix
    aiohttp
    asyncpg
    python-olm
    unpaddedbase64
    pycryptodome
    base58
  ];

  meta = {
    description = "Matrix calendar bot bridge for n8n using mautrix-python";
    mainProgram = "calendar-bot";
    license = lib.licenses.mit;
    maintainers = [ ];
  };
}
