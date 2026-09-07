{
  lib,
  rustPlatform,
  fetchFromGitHub,
  stdenv,
  pkg-config,
  openssl,
  dbus,
  sqlite,
}:
let
  # Unreleased pimalaya tools built from git; bump with ./update.sh
  sources = lib.importJSON ./sources.json;
  mk =
    pname:
    {
      rev,
      hash,
      cargoHash,
      description,
    }:
    rustPlatform.buildRustPackage {
      inherit pname cargoHash;
      version = "0-unstable-${builtins.substring 0 7 rev}";
      src = fetchFromGitHub {
        owner = "pimalaya";
        repo = pname;
        inherit rev hash;
      };
      nativeBuildInputs = [ pkg-config ];
      buildInputs = [
        openssl
        sqlite
      ]
      ++ lib.optional (pname == "carillon" && stdenv.hostPlatform.isLinux) dbus;
      # upstream test-suites need network / fixtures
      doCheck = false;
      meta = {
        inherit description;
        homepage = "https://github.com/pimalaya/${pname}";
        license = lib.licenses.mit;
        mainProgram = pname;
      };
    };
in
lib.mapAttrs mk sources
