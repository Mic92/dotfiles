{
  lib,
  python3,
  msmtp,
  khal,
  vdirsyncer,
}:

python3.pkgs.buildPythonApplication {
  pname = "ical-cli";
  version = "0.1.0";

  src = ./.;

  pyproject = true;

  build-system = with python3.pkgs; [
    setuptools
  ];

  dependencies = with python3.pkgs; [
    icalendar
    python-dateutil
    pytz
  ];

  makeWrapperArgs = [
    "--prefix PATH : ${
      lib.makeBinPath [
        msmtp
        khal
        vdirsyncer
      ]
    }"
  ];

  nativeCheckInputs = with python3.pkgs; [
    pytestCheckHook
  ];

  meta = {
    description = "Command-line tools for working with iCalendar files";
    homepage = "https://github.com/Mic92/dotfiles";
    license = lib.licenses.mit;
    mainProgram = "import-calendar-invite";
  };
}
