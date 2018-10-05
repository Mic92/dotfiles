{ stdenv, fetchurl, fetchpatch, optipng }:

stdenv.mkDerivation rec {
  name = "phpldapadmin-${version}";
  version = "1.2.3";

  src = fetchurl {
    url = "https://downloads.sourceforge.net/project/phpldapadmin/phpldapadmin-php5/${version}/phpldapadmin-${version}.tgz";
    sha256 = "0n7dhp2a7n1krmnik3pb969jynsmhghmxviivnckifkprv1zijmf";
  };

  patches = [
    (fetchpatch {
      name = "disable-mcrypt.patch";
      url = "https://git.archlinux.org/svntogit/community.git/plain/trunk/disable-mcrypt.patch?h=packages/phpldapadmin&id=2e58a4464e310a9f5c5f7da4d92a2e9647568ad4";
      sha256 = "0avr8c28f2rxknjs1n6986gna0pb6rkzpjkdfqh77q5k4kgixx5w";
    })
    (fetchpatch {
      name = "phpldapadmin-1.2.3-php5_5.patch";
      url = "https://git.archlinux.org/svntogit/community.git/plain/trunk/phpldapadmin-1.2.3-php5_5.patch?h=packages/phpldapadmin&id=2e58a4464e310a9f5c5f7da4d92a2e9647568ad4";
      sha256 = "04iwimi52g2zi7g7mw8v5w14vbll8v71lq9gngq4m6lb5hcsls6h";
    })
    (fetchpatch {
      name = "phpldapadmin-1.2.3-php7_2.patch";
      url = "https://git.archlinux.org/svntogit/community.git/plain/trunk/phpldapadmin-1.2.3-php7_2.patch?h=packages/phpldapadmin&id=2e58a4464e310a9f5c5f7da4d92a2e9647568ad4";
      sha256 = "00xgf5spd0zkw01dbl9kv58c2rd79x2j0fdkzprlc3k5g2ahcxp8";
    })
    (fetchpatch {
      name = "sort-in-templates.patch";
      url = "https://git.archlinux.org/svntogit/community.git/plain/trunk/sort-in-templates.patch?h=packages/phpldapadmin&id=2e58a4464e310a9f5c5f7da4d92a2e9647568ad4";
      sha256 = "1b7yw71z7cj8gj40lldhqpk2d0l9lyijzw6fjrdnv7nljni6ba2i";
    })
  ];

  buildInputs = [ optipng ];

  installPhase = ''
		find . -name '*.png' -print0 | xargs -0 optipng -quiet -force -fix

		mkdir -p $out/share/phpldapadmin
		cp -ra . $out/share/phpldapadmin
		ln -s /etc/phpldapadmin/config.php $out/share/phpldapadmin/config/config.php
	'';
}
