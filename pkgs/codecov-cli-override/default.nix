{ codecov-cli, fetchFromGitHub }:

# The upstream package tries to fix SSH URL issues with GIT_CONFIG_* variables,
# but those don't work properly during fetchSubmodules. Instead, we fetch
# without submodules and then manually fix the .gitmodules file before
# initializing submodules.
codecov-cli.overrideAttrs (old: {
  src =
    (fetchFromGitHub {
      owner = "codecov";
      repo = "codecov-cli";
      tag = "v${old.version}";
      hash = "sha256-R1GFQ81N/e2OX01oSs8Xs+PM0JKVZofiUPADVdxCzWk=";
      fetchSubmodules = true;
    }).overrideAttrs
      (oldSrc: {
        # Fix .gitmodules to use HTTPS instead of SSH URLs before fetching submodules
        postFetch = (oldSrc.postFetch or "") + ''
          # Replace SSH URLs with HTTPS URLs in .gitmodules
          if [ -f "$out/.gitmodules" ]; then
            sed -i 's|git@github\.com:|https://github.com/|g' "$out/.gitmodules"
          fi
        '';
        # Set git config to use HTTPS for github during submodule init
        GIT_CONFIG_COUNT = "1";
        GIT_CONFIG_KEY_0 = "url.https://github.com/.insteadOf";
        GIT_CONFIG_VALUE_0 = "git@github.com:";
      });
})
