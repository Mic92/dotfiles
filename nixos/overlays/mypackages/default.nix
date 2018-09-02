self: super:
rec {
  remote-pdb = super.python2Packages.buildPythonApplication rec {
    pname = "remote-pdb";
    version = "1.2.0";
    src = super.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "00aicmlrw3q31s26h8549n71p75p1nr0jcv40fyx47axw96kdbk1";
    };
  };

  #mosh = super.mosh.overrideDerivation (old: {
  #  name = "mosh-ssh-agent";
  #  src = super.fetchFromGitHub {
  #    owner = "mobile-shell";
  #    repo = "mosh";
  #    rev = "968f3ccba04faf3a5b12d583128ce7450b006742";
  #    sha256 = "1dyraknc9wwfb097ixryzjj86d60zz4yi0av0fq07p3bjz8f1sd9";
  #  };
  #});

}
