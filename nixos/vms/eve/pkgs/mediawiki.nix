{ stdenv, fetchurl }: 
stdenv.mkDerivation rec {
  name = "mediawiki-${version}";
  majorVersion = "1.31";
  version = "${majorVersion}.1";

  src = fetchurl {
    url = "https://releases.wikimedia.org/mediawiki/${majorVersion}/mediawiki-${version}.tar.gz";
    sha256 = "13x48clij21cmysjkpnx68vggchrdasqp7b290j87xlfgjhdhnnf";
  };

  installPhase = ''
    mkdir -p $out/share/mediawiki
    cp -ra * $out/share/mediawiki
    cp ${./logo.png} $out/share/mediawiki/logo.png
    ln -s /etc/mediawiki/LocalSettings.php $out/share/mediawiki
    ln -s /var/lib/mediawiki/ $out/share/mediawiki/images
  '';

  meta = with stdenv.lib; {
    description = "Free and open source software wiki package written in PHP";
    homepage = https://www.mediawiki.org/wiki/MediaWiki;
    license = licenses.gpl2;
  };
}
