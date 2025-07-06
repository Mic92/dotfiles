{
  lib,
  python3,
  msmtp,
}:

python3.pkgs.buildPythonApplication rec {
  pname = "create-calendar-invite";
  version = "1.0.0";
  format = "other";

  src = ./.;

  propagatedBuildInputs = with python3.pkgs; [
    icalendar
    pytz
  ];

  dontBuild = true;
  dontConfigure = true;

  installPhase = ''
    runHook preInstall

    install -Dm755 create-calendar-invite.py $out/bin/create-calendar-invite

    runHook postInstall
  '';

  makeWrapperArgs = [
    "--prefix"
    "PATH"
    ":"
    (lib.makeBinPath [ msmtp ])
  ];

  meta = with lib; {
    description = "Generate and send ICS calendar invites via msmtp";
    mainProgram = "create-calendar-invite";
  };
}
