with import <nixpkgs> {};
  stdenv.mkDerivation {
    name = "env";
    buildInputs =
      [
        bashInteractive
        (stdenv.mkDerivation {
          name = "gio-modules";
          unpackPhase = ":";
          installPhase = ''
            mkdir -p $out/nix-support
            cat > $out/nix-support/setup-hook <<'EOF'
            gioExtraModules=()
            export-gio-modules() {
                if [ -d "$1"/lib/gio/modules ] && [ -n "$(ls -A $1/lib/gio/modules)" ] ; then
                    gioExtraModules+=("$1/lib/gio/modules")
                fi
            }

            addEnvHooks "$targetOffset" export-gio-modules
            EOF
          '';
        })
      ]
      ++ dino.buildInputs
      ++ dino.nativeBuildInputs;

    shellHook = ''
      export XDG_DATA_DIRS=$XDG_DATA_DIRS:$GSETTINGS_SCHEMAS_PATH
      export GIO_EXTRA_MODULES=$(IFS=:; echo "''${gioExtraModules[*]}")
    '';
  }
