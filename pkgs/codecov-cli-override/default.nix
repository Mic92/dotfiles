{ codecov-cli, fetchgit }:

# The codecov-cli package has git submodules with SSH URLs that fail in the Nix sandbox.
# We use fetchgit with explicit git config to rewrite SSH URLs to HTTPS.
codecov-cli.overrideAttrs (old: {
  src = fetchgit {
    url = "https://github.com/codecov/codecov-cli";
    rev = "refs/tags/v${old.version}";
    hash = "sha256-R1GFQ81N/e2OX01oSs8Xs+PM0JKVZofiUPADVdxCzWk=";
    fetchSubmodules = true;
    # Configure git to rewrite SSH URLs to HTTPS before fetching submodules
    GIT_CONFIG_COUNT = "1";
    GIT_CONFIG_KEY_0 = "url.https://github.com/.insteadOf";
    GIT_CONFIG_VALUE_0 = "git@github.com:";
  };
})
