{ stdenv, python3, fetchFromGitHub }:

let
  gatt = with python3.pkgs; buildPythonPackage rec {
    pname = "gatt";
    version = "0.2.7";

    src = fetchPypi {
      inherit pname version;
      sha256 = "626d9de24a178b6eaff78c31b0bd29f962681da7caf18eb20363f6288d014e3a";
    };

    propagatedBuildInputs = [ dbus-python pygobject3 ];
  };
in stdenv.mkDerivation {
  name = "hue-ble-ctl";
  src = fetchFromGitHub {
    owner = "Mic92";
    repo = "hue-ble-ctl";
    rev = "335fe388566baef2daceed11e455a7b46aaf6c2a";
    sha256 = "0rnky829k4d08h61gfkzn83sm226qcmank5x9lyadfd8ypxqgyb5";
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
