{
  pkgs ? import <nixpkgs> { },
}:

let
  vcal = pkgs.callPackage ./default.nix { };
  types-icalendar = pkgs.callPackage ./types-icalendar.nix {
    python = pkgs.python3;
  };
in
pkgs.mkShell {
  inputsFrom = [ vcal ];

  buildInputs = with pkgs; [
    # Development tools
    python3.pkgs.mypy
    python3.pkgs.ruff
    python3.pkgs.pytest

    # Type stubs
    python3.pkgs.types-pytz
    python3.pkgs.types-python-dateutil
    types-icalendar

    # Runtime dependencies
    msmtp
    khal
    vdirsyncer
  ];
}
