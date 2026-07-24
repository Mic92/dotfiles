{
  lib,
  buildGoModule,
  fetchFromGitHub,
}:

buildGoModule rec {
  pname = "herdr-sesh";
  version = "0.5.0";

  src = fetchFromGitHub {
    owner = "fullerzz";
    repo = "herdr-plugin-sesh";
    rev = "v${version}";
    hash = "sha256-IGLMExUtNI8ybwY0tOVzhxZSFl5SJgu98DW+kvcBTyY=";
  };

  vendorHash = "sha256-TnfuQetN3KaRsB5r1bTCcQwOw6kqYVjzKb2aWkz6C0A=";

  subPackages = [ "cmd/herdr-sesh" ];

  ldflags = [ "-X=github.com/fullerzz/herdr-plugin-sesh/internal/app.Version=${version}" ];

  # Ship as a herdr plugin directory: manifest at the root, binary under bin/,
  # matching the "./bin/herdr-sesh" commands in the manifest.
  postInstall = ''
    cp herdr-plugin.toml $out/
  '';

  meta = {
    description = "Sesh-style workspace picker for herdr";
    homepage = "https://github.com/fullerzz/herdr-plugin-sesh";
    license = lib.licenses.mit;
    mainProgram = "herdr-sesh";
  };
}
