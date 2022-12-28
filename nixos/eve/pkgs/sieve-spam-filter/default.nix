{ stdenv
, makeWrapper
, rspamd
,
}:
stdenv.mkDerivation {
  name = "sieve-rspamd-filter";
  nativeBuildInputs = [ makeWrapper ];
  src = ./src;

  installPhase = ''
    for sieve in $src/*.sieve; do
      install -D "$sieve" "$out/share/sieve-rspamd-filter/$(basename $sieve)"
    done

    mkdir $out/bin
    cat > $out/bin/learn-spam.sh <<'EOF'
    #!/bin/sh
    exec ${rspamd}/bin/rspamc -h /run/rspamd.sock learn_spam
    EOF
    cat > $out/bin/learn-ham.sh <<'EOF'
    #!/bin/sh
    exec ${rspamd}/bin/rspamc -h /run/rspamd.sock learn_ham
    EOF
    chmod +x $out/bin/*.sh
  '';
}
