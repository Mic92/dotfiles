{
  lib,
  stdenv,
  sbcl,
  fetchFromGitHub,
}:
let
  trivial-glob = fetchFromGitHub {
    owner = "fukamachi";
    repo = "trivial-glob";
    rev = "0c2675d9452ed164970f2fc4a0e41784ebee819a";
    hash = "sha256-XkeQXfKk7zcEuj4Q9eOve4d3hern0kyD2k3IamoZ6/w=";
  };
  lisp = sbcl.withPackages (p: [
    p.alexandria
    p.cl-interpol
    p.cl-ppcre
    p.eclector
    p.flexi-streams
  ]);
in
stdenv.mkDerivation {
  pname = "mallet";
  version = "0.9.2";
  src = fetchFromGitHub {
    owner = "fukamachi";
    repo = "mallet";
    tag = "0.9.2";
    hash = "sha256-9LVmkzsEGQOgKwRh5Nofu/D0uNEH21bGtPsTm/D0ifw=";
  };
  patches = [
    # Lets the metric rules measure macros such as the brain's defn.
    ./definers.patch
    ./struct-docstring-colons.patch
  ];
  nativeBuildInputs = [ lisp ];
  buildPhase = ''
    runHook preBuild
    export HOME=$TMPDIR XDG_CACHE_HOME=$TMPDIR/cache
    # trivial-glob uses define-package :local-nicknames, newer than the
    # UIOP that SBCL ships. Mallet vendors a newer one.
    sbcl --noinform --non-interactive \
      --eval '(require :asdf)' \
      --eval '(load ".bundle-libs/local-projects/asdf/uiop/package.lisp")' \
      --eval '(push #p"${trivial-glob}/" asdf:*central-registry*)' \
      --eval '(asdf:load-asd (truename "mallet.asd"))' \
      --eval '(asdf:make "mallet/executable")'
    runHook postBuild
  '';
  installPhase = ''
    install -Dm755 mallet $out/bin/mallet
  '';
  # A dumped SBCL image: stripping would cut off the core.
  dontStrip = true;
  meta = {
    description = "Common Lisp linter that catches real mistakes, not style";
    homepage = "https://github.com/fukamachi/mallet";
    license = lib.licenses.mit;
    mainProgram = "mallet";
  };
}
