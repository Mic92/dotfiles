{
  lib,
  stdenv,
  python3,
  makeWrapper,
  # Calendar tools
  khal,
  vdirsyncer,
  todoman,
  # Email tools
  notmuch,
  afew,
  mblaze,
  msmtp,
  isync,
  # Contacts
  khard,
  # Supporting tools
  rbw,
  coreutils,
  gnugrep,
  gnused,
  gawk,
  jq,
  findutils,
  bash,
  util-linux,
  ncurses,
  # Email sync
  email-sync,
  # AI tools
  pi,
  db-cli,
  kagi-search,
  crabfit-cli,
  # Sandboxing (Linux only)
  bubblewrap,
}:

let
  # Build a minimal PATH with only calendar/email related tools
  toolsPath = lib.makeBinPath [
    # Calendar
    khal
    vdirsyncer
    todoman
    # Email
    notmuch
    afew
    mblaze
    msmtp
    isync
    # Contacts
    khard
    # Auth
    rbw
    # Basic utils
    coreutils
    gnugrep
    gnused
    gawk
    jq
    findutils
    bash
    util-linux
    ncurses
    # Email sync
    email-sync
    # Skills dependencies
    db-cli
    kagi-search
    crabfit-cli
  ];

  runtimeDeps = lib.optionals stdenv.isLinux [ bubblewrap ];
in
python3.pkgs.buildPythonApplication {
  pname = "pim";
  version = "0.2.0";
  src = ./.;
  format = "other";

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    install -D -m 0755 pim.py $out/bin/pim

    wrapProgram $out/bin/pim \
      --set PIM_TOOLS_PATH ${lib.escapeShellArg toolsPath} \
      --set PIM_PI_BIN ${pi}/bin/pi \
      --prefix PATH : ${lib.makeBinPath runtimeDeps}
  '';

  meta = with lib; {
    description = "Sandboxed AI assistant for calendar, email, contacts, and travel planning";
    license = licenses.mit;
    platforms = platforms.all;
    mainProgram = "pim";
  };
}
