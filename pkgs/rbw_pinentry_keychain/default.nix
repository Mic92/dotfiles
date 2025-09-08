{
  python3,
  writeScriptBin,
}:

let
  pinentryScript = ./pinentry_keychain.py;
in
writeScriptBin "rbw-pinentry-keychain" ''
  #!${python3}/bin/python3
  ${builtins.readFile pinentryScript}
''
