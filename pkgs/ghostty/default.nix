# Ghostty with fix for build failures on high-core-count AMD systems
{ ghostty, stdenv }:

ghostty.overrideAttrs (_oldAttrs: {
  # Force local builds on Linux only (Darwin builds are slow enough to warrant remote builds)
  preferLocalBuild = stdenv.isLinux;
})
