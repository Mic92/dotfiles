# Regression test for ghidra-cli + nixpkgs' Ghidra 12.
#
# Exercises the full import + decompile round-trip so we notice when a Ghidra
# bump in nixpkgs breaks the Java bridge (it surfaces as
# "ClassNotFoundException: GhidraCliBridge"; the real javac error is in
# ~/.config/*ghidra*/*/application.log).
{
  stdenv,
  runCommand,
  ghidra-cli,
}:

runCommand "ghidra-cli-test"
  {
    nativeBuildInputs = [
      stdenv.cc
      ghidra-cli
    ];
    meta.timeout = 30 * 60;
  }
  ''
    export HOME=$PWD/home
    export XDG_CONFIG_HOME=$HOME/.config
    export XDG_CACHE_HOME=$HOME/.cache
    export XDG_DATA_HOME=$HOME/.local/share

    # Ghidra 12 rejects project path elements starting with '.', so the default
    # ~/.cache/ghidra-cli/projects aborts every import. Use a dot-free dir.
    # GHIDRA_PROJECT_DIR wins over the cache dir; ghidra-cli won't create it.
    export GHIDRA_PROJECT_DIR=$PWD/projects
    mkdir -p "$GHIDRA_PROJECT_DIR"

    cat > hello.c <<'EOF'
    #include <stdio.h>
    int main(void) { puts("hello-from-ghidra-cli"); return 0; }
    EOF
    cc -O0 hello.c -o hello

    echo ">>> import"
    ghidra import ./hello --project regress

    echo ">>> decompile"
    # Mach-O prefixes C symbols with an underscore, ELF does not.
    ghidra decompile ${if stdenv.hostPlatform.isDarwin then "_main" else "main"} \
      --project regress --program hello | tee decomp.json

    # The decompiler must recover both the call and the string literal.
    grep -q 'puts' decomp.json
    grep -q 'hello-from-ghidra-cli' decomp.json

    ghidra stop --project regress

    touch $out
  ''
