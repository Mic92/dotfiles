{ stdenv, buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  name = "fuidshift-${version}";
  version = "20150809-${stdenv.lib.strings.substring 0 7 rev}";
  rev = "4a8d24f7bcbfa539f06c021ed0ba13823ce87678";

  goPackagePath = "github.com/Mic92/fuidshift";

  src = fetchFromGitHub {
    inherit rev;
    owner = "Mic92";
    repo = "fuidshift";
    sha256 = "0g2zvn7q4n8gjwk996hfa4fbf7n5zlii1sv16lr6r20n4m25f6yi";
  };

  goDeps = ./fuidshift-deps.nix;
}
