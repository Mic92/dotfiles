{ stdenv
, python3
, fetchFromGitHub
,
}:
let
  gatt = with python3.pkgs;
    buildPythonPackage rec {
      pname = "gatt";
      version = "0.2.7";

      src = fetchPypi {
        inherit pname version;
        sha256 = "626d9de24a178b6eaff78c31b0bd29f962681da7caf18eb20363f6288d014e3a";
      };

      propagatedBuildInputs = [ dbus-python pygobject3 ];
    };
in
stdenv.mkDerivation {
  name = "hue-ble-ctl";
  src = fetchFromGitHub {
    owner = "Mic92";
    repo = "hue-ble-ctl";
    rev = "d7750a78c38d529eaf34b5ad300e929e8fbec2c2";
    sha256 = "sha256-GE+E1q8aYYVQfPXZx981dzOTzn/gmryRIIlpIxdEycI=";
  };
  pythonPath = [ gatt ];
  nativeBuildInputs = [
    python3.pkgs.wrapPython
  ];
  installPhase = ''
    install -m755 -D hue-ble-ctl.py $out/bin/hue-ble-ctl
  '';
  postFixup = "wrapPythonPrograms";
}
