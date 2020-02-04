{ stdenv, fetchpatch, fetchurl, weechat, gmp }:
weechat.override {
  configure = { availablePlugins, ... }: {
    plugins = [
      (availablePlugins.python.withPackages (ps: [
        (ps.potr.overridePythonAttrs (oldAttrs: {
          propagatedBuildInputs = [
            (ps.buildPythonPackage rec {
              name = "pycrypto-${version}";
              version = "2.6.1";

              src = fetchurl {
                url = "mirror://pypi/p/pycrypto/${name}.tar.gz";
                sha256 = "0g0ayql5b9mkjam8hym6zyg6bv77lbh66rv1fyvgqb17kfc1xkpj";
              };

              patches = stdenv.lib.singleton (fetchpatch {
                name = "CVE-2013-7459.patch";
                url = "https://anonscm.debian.org/cgit/collab-maint/python-crypto.git"
                  + "/plain/debian/patches/CVE-2013-7459.patch?h=debian/2.6.1-7";
                sha256 = "01r7aghnchc1bpxgdv58qyi2085gh34bxini973xhy3ks7fq3ir9";
              });

              buildInputs = [ gmp ];

              preConfigure = ''
                sed -i 's,/usr/include,/no-such-dir,' configure
                sed -i "s!,'/usr/include/'!!" setup.py
              '';
            })
          ];
        }
        )
      )]))
      (availablePlugins.perl)
      (availablePlugins.lua)
    ];
  };
}
