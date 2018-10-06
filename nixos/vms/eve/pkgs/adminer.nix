{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  name = "adminer-${version}";
  version = "4.6.3";

  src = fetchurl {
    url = "https://www.adminer.org/static/download/${version}/${name}.php";
    sha256 = "1qi41wgxp9nk7zdqq17849kad8apivdm29184v7h210x5np60jma";
  };

  unpackPhase = ":";

  installPhase = ''
    install -Dm0644 "${src}" \
      "$out/share/adminer/index.php"
  '';

  meta = with stdenv.lib; {
    description = "A full-featured Database management tool written in PHP.";
    homepage = https://www.adminer.org;
    license = licenses.asl20;
  };
}
