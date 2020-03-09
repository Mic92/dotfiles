{ stdenv, fetchurl }:
stdenv.mkDerivation rec {
  pname = "mediawiki";
  majorVersion = "1.34";
  version = "${majorVersion}.0";

  src = fetchurl {
    url = "https://releases.wikimedia.org/mediawiki/${majorVersion}/mediawiki-${version}.tar.gz";
    sha256 = "1lckjnharwxh9xb7gxdxrkb0r3xgd0dh4019cnbixn5mmzgc696y";
  };

  installPhase = ''
    mkdir -p $out/share/mediawiki
    cp -ra * $out/share/mediawiki
    cp ${./logo.png} $out/share/mediawiki/logo.png
    ln -s /etc/mediawiki/LocalSettings.php $out/share/mediawiki
    rm -rf $out/share/mediawiki/images
    ln -s /var/lib/mediawiki/uploads $out/share/mediawiki/images
  '';

  meta = with stdenv.lib; {
    description = "Free and open source software wiki package written in PHP";
    homepage = https://www.mediawiki.org/wiki/MediaWiki;
    license = licenses.gpl2;
  };
}
