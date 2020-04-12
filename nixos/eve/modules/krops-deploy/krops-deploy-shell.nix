{ stdenv, python3, mypy }:

stdenv.mkDerivation {
  name = "krops-deploy-shell";
  src = ./.;
  nativeBuildInputs = [
    mypy python3.pkgs.black python3.pkgs.flake8
  ];
  installPhase = ''
    install -m755 -D krops-deploy-shell.py $out/bin/krops-deploy-shell

    echo -e "\x1b[32m## run black\x1b[0m"
    LC_ALL=en_US.utf-8 black --check krops-deploy-shell.py
    echo -e "\x1b[32m## run flake8\x1b[0m"
    flake8 krops-deploy-shell.py
    echo -e "\x1b[32m## run mypy\x1b[0m"
    mypy krops-deploy-shell.py
  '';
}
