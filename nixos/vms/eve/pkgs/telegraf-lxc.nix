{ pkgconfig, lxc, fetchFromGitHub, }:

buildGoPackage {
  name = "telegraf-lxc-stats";
  goPackagePath = "github.com/Mic92/telegraf-lxc-stats";

  goDeps = ./telegraf-lxc-deps.nix;
  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ lxc ];

  src = fetchFromGitHub {
    owner  = "Mic92";
    repo   = "telegraf-lxc-stats";
    rev    = "6f34d284256276495a99684165cfcdf619316339";
    sha256 = "168vm4ak14ji1a6gmx8cwf71ivj2nnw103jbl72kqj70v4b4j105";
  };
}
