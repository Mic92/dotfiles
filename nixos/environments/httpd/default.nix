with import <nixpkgs> {}; let
  aprSrc = fetchzip {
    url = "mirror://apache/apr/apr-1.5.2.tar.bz2";
    sha256 = "02n46xndsx9kr1zvbqqxriazwabkij5i104cwkvw5ayx8l9hz65d";
  };
  aprutilSrc = fetchzip {
    url = "mirror://apache/apr/apr-util-1.5.4.tar.bz2";
    sha256 = "06cavh899a4gkmqigv14hi9xinc0j5ix32kp0lgqfbnrd4rpscs6";
  };
in
  stdenv.mkDerivation {
    name = "env";
    buildInputs =
      [
        # test dependencies
        linuxPackages_4_11.perf

        bashInteractive
        m4
        openssl
        autoreconfHook
        libtool
        pcre
        subversion
        openssl
      ]
      ++ (with perlPackages; [
        XMLDOM
        NetSSLeay
        IOSocketSSL
        IOSocketIP
        NetSSLeay
        CryptSSLeay
        AnyEvent
        LWPProtocolHttps
        Test
        TestHarness
        DateTime
        TimeHiRes
        FCGI
        HTTPDAV
        ProtocolHTTP2
      ])
      ++ apacheHttpd.buildInputs;
    nativeBuildInputs = apacheHttpd.nativeBuildInputs;
    #inherit (apacheHttpd) configureFlags;
    configureFlags = [
      "--prefix=$(readlink -f prefix)"
      "--with-apr=${apr.dev}"
      "--with-apr-util=${aprutil.dev}"
      "--with-z=${zlib.dev}"
      "--with-pcre=${pcre.dev}"
      "--enable-modules=reallyall"
      "--enable-load-all-modules"
    ];

    buildConfFlags = ["--with-apr=${aprSrc}" "--with-apr-util=${aprutilSrc}"];

    installFlags = ["DESTDIR=$(readlink -f destination)" "install"];

    testFlags = ["-apxs" "$(readlink -f ../prefix/bin/apxs)"];

    # perl -MCPAN -e 'foreach (split / /, $ARGV[0]) { CPAN::Shell->rematein("notest", "install", $_) }' $perlModules
    perlModules = ["IO::Select"];
  }
