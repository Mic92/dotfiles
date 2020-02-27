{ buildPythonPackage, flask }:

buildPythonPackage {
  name = "choose-place";
  src = (import ../../../nix/sources.nix {}).choose-place;
  propagatedBuildInputs = [ flask ];
}
