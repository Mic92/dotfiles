# Ghostty with fix for build failures on high-core-count AMD systems
{ ghostty, lib }:

ghostty.overrideAttrs (_oldAttrs: {
  # Force local builds
  preferLocalBuild = true;
})
