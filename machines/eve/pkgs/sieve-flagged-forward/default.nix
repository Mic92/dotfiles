# Forward starred/flagged emails to Janet (opencrow) via its trigger pipe.
# Dovecot's imapsieve calls the sieve script on FLAG changes; the sieve
# script pipes the message to notify-flagged.py which writes a summary to
# the trigger pipe.
{
  stdenv,
  python3,
}:
stdenv.mkDerivation {
  name = "sieve-flagged-forward";
  src = ./src;

  installPhase = ''
    for sieve in $src/*.sieve; do
      install -D "$sieve" "$out/share/sieve-flagged-forward/$(basename $sieve)"
    done

    install -Dm755 ${./notify-flagged.py} $out/bin/notify-flagged
    patchShebangs $out/bin/notify-flagged
  '';

  nativeBuildInputs = [ python3 ];
}
