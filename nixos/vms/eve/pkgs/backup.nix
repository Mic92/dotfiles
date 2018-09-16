{ stdenv, bash, ruby, makeWrapper
, borgbackup, gzip, bzip2, openssh, lxc }:
stdenv.mkDerivation {
  name = "dbbackupscripts";
  src = ./backup;
  buildInputs = [ bash ruby ];
  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    install -m755 -D backup-mysql $out/bin/backup-mysql
    install -m755 -D backup-postgres $out/bin/backup-postgres
    install -m755 -D backup-container $out/bin/backup-container

    patchShebangs .

    for bin in $out/bin/*; do
      wrapProgram $bin --prefix PATH ":" ${stdenv.lib.makeBinPath [
        borgbackup
        gzip
        bzip2
        openssh
        lxc
      ]}
    done
  '';
}
