{
  pkgs ? import <nixpkgs> { },
}:

let
  pythonEnv = pkgs.python3.withPackages (
    ps: with ps; [
      (ps.toPythonModule pkgs.afew)
      notmuch
    ]
  );
in
pkgs.mkShell {
  buildInputs = [
    pythonEnv
  ];

  shellHook = ''
    export PYTHONPATH="${pythonEnv}/${pythonEnv.sitePackages}:$PWD:$PYTHONPATH"
  '';
}
