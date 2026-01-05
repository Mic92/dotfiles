{ mcp, lib, stdenv }:

# Fix for https://github.com/NixOS/nixpkgs/commit/fa82e35
# The postPatch in the base package tries to patch files that no longer exist in version 1.25.0
# This override applies the fix from upstream nixpkgs
mcp.overridePythonAttrs (oldAttrs: {
  postPatch = lib.optionalString stdenv.buildPlatform.isDarwin ''
    # time.sleep(0.1) feels a bit optimistic and it has been flaky whilst
    # testing this on macOS under load.
    substituteInPlace \
      "tests/client/test_stdio.py" \
      --replace-fail "time.sleep(0.1)" "time.sleep(1)"
  '';
})
