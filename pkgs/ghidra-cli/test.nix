# Regression test for ghidra-cli + nixpkgs' Ghidra 12.
#
# Upstream v0.1.10 is broken against Ghidra 12 (akiselev/ghidra-cli#10):
#   1. The Java bridge is dropped under ~/.config which Ghidra's Bnd-based
#      OSGi compiler refuses to scan, so analyzeHeadless dies with
#      "ClassNotFoundException: Failed to get OSGi bundle containing script".
#   2. The bridge uses the removed CParseResults API and would not compile
#      even if (1) were fixed.
#
# Both are addressed by ./ghidra-12-compat.patch.  This derivation exercises
# the full import + decompile round-trip so we notice when a future Ghidra
# bump in nixpkgs breaks the bridge again.
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

    # ghidra-cli does not create the project root on first import; doctor only
    # reports it.  Pre-create it so the headless analyzer does not bail out.
    mkdir -p "$XDG_CACHE_HOME/ghidra-cli/projects"

    cat > hello.c <<'EOF'
    #include <stdio.h>
    int main(void) { puts("hello-from-ghidra-cli"); return 0; }
    EOF
    cc -O0 hello.c -o hello

    echo ">>> import"
    ghidra import ./hello --project regress

    echo ">>> decompile"
    ghidra decompile main --project regress --program hello | tee decomp.json

    # The decompiler must recover both the call and the string literal.
    grep -q 'puts' decomp.json
    grep -q 'hello-from-ghidra-cli' decomp.json

    ghidra stop --project regress

    touch $out
  ''
