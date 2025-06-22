{
  pkgs ? import <nixpkgs> { },
}:

let
  pythonEnv = pkgs.python3.withPackages (
    ps: with ps; [
      (ps.toPythonModule pkgs.afew)
      notmuch
      pytest
    ]
  );
in
pkgs.mkShell {
  buildInputs = [
    pythonEnv
    pkgs.mypy
    pkgs.claude-code
  ];

  shellHook = ''
    export PYTHONPATH="${pythonEnv}/${pythonEnv.sitePackages}:$PWD:$PYTHONPATH"
  '';
}
