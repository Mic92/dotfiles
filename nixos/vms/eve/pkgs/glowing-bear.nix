{ stdenv, fetchFromGitHub }: 

fetchFromGitHub {
  owner = "glowing-bear";
  repo = "glowing-bear";
  rev = "0.7.0";
  sha256 = "1za6ayjqw7s0mdc8l5wwjnk40974zvrmhy940x1f83cnrh7xz6j0";

  meta = with stdenv.lib; {
    description = "A web client for WeeChat";
    homepage = "https://github.com/glowing-bear/glowing-bear";
    license = licenses.gpl3;
  };
}
