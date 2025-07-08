{
  lib,
  python3,
  msmtp,
  khal,
  vdirsyncer,
}:

python3.pkgs.buildPythonApplication {
  pname = "vcal";
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
    "--suffix PATH : ${
      lib.makeBinPath [
        msmtp
        khal
        vdirsyncer
      ]
    }"
  ];

  nativeCheckInputs = with python3.pkgs; [
    pytestCheckHook
    mypy
    types-pytz
    types-python-dateutil
    (callPackage ./types-icalendar.nix { })
  ];

  meta = {
    description = "vCalendar/iCalendar management tool for creating, importing, and replying to calendar invitations";
    homepage = "https://github.com/Mic92/dotfiles";
    license = lib.licenses.mit;
    mainProgram = "vcal";
  };
}
