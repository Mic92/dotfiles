{ stdenv, netcat-openbsd, writeScriptBin }:

writeScriptBin "untilport" ''
  #!${stdenv.shell}
  set -euf

  usage() {
    echo 'untiport $target $port'
    echo 'Sleeps until the destinated port is reachable.'
    echo 'ex: untilport google.de 80 && echo "google is now reachable"'
  }


  if [ $# -ne 2 ]; then
    usage
  else
    until ${netcat-openbsd}/bin/nc -z "$@"; do sleep 1; done
  fi
''
