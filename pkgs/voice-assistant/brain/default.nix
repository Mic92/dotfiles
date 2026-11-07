{
  lib,
  stdenv,
  symlinkJoin,
  sbcl,
  socat,
  git,
  mallet,
  writeShellApplication,
}:
let
  lisp = sbcl.withPackages (p: [
    p.coalton
    p.alexandria
    p.trivia
    p.com_dot_inuoe_dot_jzon
    p.parachute
    p.cl-ppcre
  ]);
  src = lib.fileset.toSource {
    # The golden file is shared with the Rust side.
    root = ../.;
    fileset = lib.fileset.unions [
      ../tests/golden.tsv
      ./voice-assistant-brain.asd
      ./src
      ./skills
      ./tests
      ./build.lisp
      ./deps.lisp
      ./dev.lisp
    ];
  };
  brain = stdenv.mkDerivation {
    pname = "voice-assistant-brain";
    version = "0.1.0";
    inherit src;
    nativeBuildInputs = [ lisp ];
    # ASDF writes fasls to the cache dir.
    buildPhase = ''
      export HOME=$TMPDIR XDG_CACHE_HOME=$TMPDIR/cache
      cd brain
      sbcl --noinform --non-interactive --load build.lisp va-brain
      sbcl --noinform --non-interactive --load deps.lisp deps.core
    '';
    installPhase = ''
      install -Dm755 va-brain $out/bin/va-brain
      install -Dm644 deps.core $out/lib/va-brain/deps.core
      install -Dm644 dev.lisp $out/lib/va-brain/dev.lisp
    '';
    # The executable carries its own runtime and core.
    dontStrip = true;
  };
  sock = name: ''"''${XDG_RUNTIME_DIR:-/tmp}/voice-assistant/${name}.sock"'';
  va-eval = writeShellApplication {
    name = "va-eval";
    runtimeInputs = [ socat ];
    text = ''
      sock=${sock "dev"}
      if [ "''${1:-}" = --live ]; then
        sock=${sock "brain"}
        shift
      fi
      if [ "$#" -eq 0 ]; then
        echo "usage: va-eval [--live] FORM..." >&2
        exit 2
      fi
      printf '%s\n' "$*" | socat -t 60 - "UNIX-CONNECT:$sock"
    '';
  };
  va-check = writeShellApplication {
    name = "va-check";
    runtimeInputs = [
      va-eval
      mallet
      git
    ];
    text = ''
      cd "''${VA_BRAIN_SRC:-$(git rev-parse --show-toplevel)/pkgs/voice-assistant/brain}"
      status=0
      mallet src skills tests ./*.lisp ./*.asd || status=1
      va-eval '(va:check)' | tee /dev/stderr | tail -1 | grep -q '^T$' || status=1
      exit "$status"
    '';
  };
  va-dev = writeShellApplication {
    name = "va-dev";
    runtimeInputs = [
      lisp
      git
    ];
    text = ''
      VA_BRAIN_SRC=''${VA_BRAIN_SRC:-$(git rev-parse --show-toplevel)/pkgs/voice-assistant/brain}
      export VA_BRAIN_SRC
      exec sbcl --core ${brain}/lib/va-brain/deps.core --noinform \
        --disable-debugger --load ${brain}/lib/va-brain/dev.lisp
    '';
  };
in
symlinkJoin {
  name = "voice-assistant-brain-0.1.0";
  paths = [
    brain
    va-eval
    va-check
    va-dev
  ];
}
