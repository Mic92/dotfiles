# Pinned noctalia plugin catalogs, plus the plugin API type definitions
# used to check our own plugins.
{
  applyPatches,
  fetchFromGitHub,
  fetchurl,
}:
{
  # official-plugins main tracks noctalia's dev branch; this is the last
  # revision at plugin_api 34, which is what noctalia 5.3.0 supports.
  # Bump together with pkgs.noctalia.
  apiDefinitions = fetchurl {
    url = "https://raw.githubusercontent.com/noctalia-dev/official-plugins/734850fd1ee3badc2a18a515df31f36267852e46/noctalia.d.luau";
    hash = "sha256-3I/njZGMguFnuKiH+UvMO7srOTc7P9A2zqzzqFJJh2o=";
  };

  official = fetchFromGitHub {
    owner = "noctalia-dev";
    repo = "official-plugins";
    rev = "10dd0255e45297e5d8a41c78f08387a7bbd21b1c";
    hash = "sha256-a4hRxpo88vn4G5I7IE4WW9JpSxD3Bw2daBI5C+DkT4c=";
  };
  community = applyPatches {
    name = "community-plugins";
    src = fetchFromGitHub {
      owner = "noctalia-dev";
      repo = "community-plugins";
      rev = "f1f51da23a2867e6447650a9fca5d671fc30bec8";
      hash = "sha256-z9ks63WDvH2a2lDmj2ey5y++e1E+HQnsv0dNVUOfPyk=";
    };
    # phone-connect: the dimmed device glyph already signals offline; drop
    # the extra "Offline" label that has no setting to turn it off.
    postPatch = ''
      substituteInPlace phone-connect/widget.luau \
        --replace-fail 'elseif not reachable and d.isPaired then' 'elseif false then'
    '';
  };
}
