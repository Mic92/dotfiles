{
  gtk4,
  webkitgtk_6_0,
  lib,
  fetchFromGitea,
  gnumake,
  cmake,
  clang-tools,
  pkg-config,
  stdenv,
  ...
}:

stdenv.mkDerivation {
  pname = "webview";
  version = "nightly";

  # We add the function id to the promise to be able to cancel it through the UI
  # We disallow remote connections from the UI on Linux
  # TODO: Disallow remote connections on MacOS

  src = fetchFromGitea {
    domain = "git.clan.lol";
    owner = "clan";
    repo = "webview";
    rev = "d83c3ebffb76f25b3a9e37d59237c5d8e94060a2";
    hash = "sha256-YAfH1KCw4r2WPvBQho2ypAVH+/c/a05SsEDUYKGadFI=";
  };

  outputs = [
    "out"
    "dev"
  ];

  enableParallelBuilding = true;

  cmakeFlags = [
    "-DWEBVIEW_BUILD_TESTS=OFF"
  ];

  # Dependencies used during the build process, if any
  nativeBuildInputs = [
    gnumake
    cmake
    clang-tools
    pkg-config
  ];

  buildInputs = lib.optionals stdenv.isLinux [
    webkitgtk_6_0
    gtk4
  ];

  meta = with lib; {
    description = "Tiny cross-platform webview library for C/C++. Uses WebKit (GTK/Cocoa) and Edge WebView2 (Windows)";
    homepage = "https://github.com/webview/webview";
    license = licenses.mit;
    platforms = platforms.linux ++ platforms.darwin;
  };
}
