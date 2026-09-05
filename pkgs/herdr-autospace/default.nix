{
  lib,
  stdenvNoCC,
  python3,
  git,
  makeWrapper,
}:

stdenvNoCC.mkDerivation {
  pname = "herdr-autospace";
  version = "0.1.0";

  src = lib.cleanSource ./.;

  nativeBuildInputs = [ makeWrapper ];
  buildInputs = [ python3 ];

  # Ship as a herdr plugin directory: manifest at the root, script under bin/.
  # herdr itself comes from the calling environment (the running server's CLI).
  installPhase = ''
    install -Dm755 herdr_autospace.py $out/bin/herdr-autospace
    patchShebangs $out/bin
    wrapProgram $out/bin/herdr-autospace --prefix PATH : ${lib.makeBinPath [ git ]}
    cp herdr-plugin.toml $out/
  '';

  meta = {
    description = "Sort herdr tabs into workspaces by project directory";
    license = lib.licenses.mit;
    mainProgram = "herdr-autospace";
  };
}
