{ luaPackages, fetchFromGitHub, openldap, lua, pkgconfig }:

luaPackages.buildLuaPackage rec {
  name = "lualdap-${version}";
  version = "1.2.0";
  src = fetchFromGitHub {
    owner = "mic92";
    repo = "lualdap";
    rev = "59cf89b55bb0b47b043634bba8129fb04186d591";
    sha256 = "08l587shiim04xnqklpc7m2p1gblf6sywzgjjz0ypgbyz0yicv09";
  };

  installFlags = [ "DESTDIR=$(out)/lib/lua/${lua.luaversion}" ];

  buildInputs = [
    openldap 
    lua
  ];

  nativeBuildInputs = [
    pkgconfig
  ];
}
