final: prev: {
  python3 = prev.python3.override {
    packageOverrides = pyfinal: pyprev: {
      mcp = pyprev.mcp.overridePythonAttrs (oldAttrs: {
        # Fix for mcp 1.25.0 - upstream removed time.sleep(0.1) from most test files
        # This matches the fix in NixOS/nixpkgs master
        postPatch = prev.lib.optionalString prev.stdenv.buildPlatform.isDarwin ''
          substituteInPlace tests/client/test_stdio.py \
            --replace-fail "time.sleep(0.1)" "time.sleep(1)"
        '';
      });
    };
  };

  python3Packages = final.python3.pkgs;
}
