with import <nixpkgs> { };
let
  pypi2nix = (python3.pkgs.buildPythonApplication rec {
    pname = "pypi2nix";
    version = "1.8.1";

    src = fetchFromGitHub {
      owner = "garbas";
      repo = "pypi2nix";
      rev = "v${version}";
      sha256 = "0w90bh7m9i5jh3pg30kzi99xfbalvkbz38iq8rs9mbsfx362p38k";
    };
    propagatedBuildInputs = with python3.pkgs; [ click requests jinja2 ];

  });
in
stdenv.mkDerivation {
  name = "env";
  buildInputs = [
    (myvim.override {
      configure = {
        packages.nixbundle = myVimBundle { python = pypy27; };
      };
    })
    bashInteractive
    (linuxPackages_latest.perf.overrideDerivation (old: {
      patches = [
        (fetchpatch {
          url = "https://github.com/Mic92/linux/commit/24898f7d288361ce5980f1c99189803edda5a706.patch";
          sha256 = "0hl08sc4smkxqzya5i1jzzkjhx0kzzr0c90k9l3i2k1g5mw9r2q9";
        })
      ];
    }))
    pypy27
    pypy27.pkgs.virtualenv
    pypy27.pkgs.jedi
    libffi
    libxml2
    libxslt
    cmake
    readline
    libtool
    glib
    pixman
    qt4
    graphviz
    binutils
    nasm
    zlib
    #(pkgs.runCommand "pypi2nix" {} ''
    #  mkdir -p $out/bin
    #  ln -s ${pypi2nix}/bin/pypy2nix $out/bin/pypi2nix
    #'')
  ];
}
