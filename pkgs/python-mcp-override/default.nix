{
  mcp,
  lib,
  stdenv,
}:

mcp.overridePythonAttrs (_old: {
  postPatch = lib.optionalString stdenv.buildPlatform.isDarwin ''
    # time.sleep(0.1) feels a bit optimistic and it has been flaky whilst
    # testing this on macOS under load.
    # Note: We need to check if the pattern exists before attempting replacement
    # to avoid failures when upstream changes their tests.
    for file in \
      "tests/client/test_stdio.py" \
      "tests/server/fastmcp/test_integration.py" \
      "tests/shared/test_ws.py" \
      "tests/shared/test_sse.py" \
      "tests/shared/test_streamable_http.py"; do
      if [ -f "$file" ] && grep -q "time\.sleep(0\.1)" "$file"; then
        substituteInPlace "$file" --replace-fail "time.sleep(0.1)" "time.sleep(1)"
      fi
    done
  '';
})
