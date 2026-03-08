{
  lib,
  stdenv,
  fetchFromGitHub,
  nodejs,
  pnpm_9,
  fetchPnpmDeps,
  pnpmConfigHook,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "n8n-nodes-paperless";
  version = "0.1.2";

  src = fetchFromGitHub {
    owner = "chezmoidotsh";
    repo = "n8n-nodes-paperless";
    rev = finalAttrs.version;
    hash = "sha256-LOmQpw1+U94jlyiIN3Xju1f92S+1afNFE4S6I6hp9VI=";
  };

  pnpmDeps = fetchPnpmDeps {
    inherit (finalAttrs) pname version src;
    pnpm = pnpm_9;
    fetcherVersion = 3;
    hash = "sha256-/z/sLcuHzvAzk1foomzof4p2HRwo+nJPM83NUlADGUc=";
  };

  nativeBuildInputs = [
    pnpmConfigHook
    pnpm_9
    nodejs
  ];

  buildPhase = ''
    runHook preBuild
    pnpm build
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/lib/node_modules/@n8n-chezmoi-sh/n8n-nodes-paperless
    cp -r dist package.json node_modules $out/lib/node_modules/@n8n-chezmoi-sh/n8n-nodes-paperless/
    runHook postInstall
  '';

  meta = {
    description = "n8n community node to manage documents with Paperless-ngx";
    homepage = "https://github.com/chezmoidotsh/n8n-nodes-paperless";
    license = lib.licenses.mit;
  };
})
