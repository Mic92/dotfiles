{ codecov-cli, fetchFromGitHub }:

# The upstream package uses SSH URLs in .gitmodules which don't work in Nix builds.
# We need to rewrite them to HTTPS URLs before fetching submodules.
codecov-cli.overrideAttrs (old: {
  src = fetchFromGitHub {
    owner = "codecov";
    repo = "codecov-cli";
    tag = "v${old.version}";
    hash = "sha256-R1GFQ81N/e2OX01oSs8Xs+PM0JKVZofiUPADVdxCzWk=";
    leaveDotGit = true;
    postFetch = ''
      cd "$out"
      # Rewrite SSH URLs to HTTPS in .gitmodules
      sed -i 's|git@github\.com:|https://github.com/|g' .gitmodules
      # Initialize and update submodules with HTTPS URLs
      git submodule update --init --recursive
      # Clean up .git to make the output deterministic
      find "$out" -name .git -print0 | xargs -0 rm -rf
    '';
  };
})
