{ stdenv, fetchzip }:

stdenv.mkDerivation rec {
  name = "rainloop-${version}";
  version = "1.12.1";

  src = fetchzip {
    url = "https://github.com/RainLoop/rainloop-webmail/releases/download/v${version}/rainloop-community-${version}.zip";
    sha256 = "1m5f5bs9mfikggc617sdq867byiy5smw3mca64jv626iyyx3rn4n";
    stripRoot = false;
  };

  installPhase = ''
    mkdir $out
    cp -r * $out
    mv $out/{data,data-factory}
    ln -sf /var/lib/rainloop $out/data
  '';

  meta = with stdenv.lib; {
    description = "Simple, modern & fast web-based email client";
    homepage = https://github.com/RainLoop/rainloop-webmail;
    license = licenses.agpl;
  };
}
