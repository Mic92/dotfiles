{ stdenv, fetchFromGitHub, pidgin, json-glib }:

stdenv.mkDerivation rec {
  name = "purple-skypeweb-${version}";
  version = "1.5";

  src = fetchFromGitHub {
    owner = "EionRobb";
    repo = "skype4pidgin";
    rev = "1.5";
    sha256 = "1bd9gd36yhrbrww0dvai9rnzxxj1c9sb4003c72wg27w12y47xfv";
  };

  makeFlags = [
    "SKYPEWEB_DEST=$(out)/lib/pidgin"
    "SKYPEWEB_ICONS_DEST=$(out)/share/pixmaps/pidgin/protocols"
    "SKYPEWEB_THEME_DEST=$(out)/share/pixmaps/pidgin/emotes/skype"
  ];

  preBuild = ''
    cd skypeweb
  '';

  buildInputs = [ pidgin json-glib ];
}
