{ codecov-cli, fetchgit }:

# The codecov-cli repository uses SSH URLs for git submodules, which don't work
# in Nix builds. We fetch without submodules first, fix the .gitmodules file to
# use HTTPS URLs, then manually initialize submodules.
codecov-cli.overrideAttrs (old: {
  src = fetchgit {
    url = "https://github.com/codecov/codecov-cli";
    rev = "refs/tags/v${old.version}";
    hash = "sha256-eL98TW4PgbJ8dTVbQSJtjxzDFZ8nxacqeEO0qakMJM0=";
    leaveDotGit = true;
    postFetch = ''
      cd "$out"
      # Fix .gitmodules to use HTTPS instead of SSH
      if [ -f .gitmodules ]; then
        sed -i 's|git@github\.com:|https://github.com/|g' .gitmodules
      fi
      # Initialize and update submodules with HTTPS URLs
      git submodule update --init --recursive
      # Remove .git directory after submodules are fetched
      rm -rf .git
      find . -name .git -exec rm -rf {} + || true
    '';
  };
})
