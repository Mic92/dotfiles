{
  lib,
  fetchFromGitHub,
  buildPythonApplication,
  feedparser,
  mastodon-py,
  setuptools,
}:

buildPythonApplication rec {
  pname = "mastodon-hnbot";
  version = "1.0.1";

  src = fetchFromGitHub {
    owner = "Mic92";
    repo = "mastodon-hnbot";
    rev = version;
    hash = "sha256-cqfpJcZMotivLvLdnE6c0dh3rDa2neXGTlUijCARBv4=";
  };

  pyproject = true;
  build-system = [ setuptools ];
  dependencies = [
    feedparser
    mastodon-py
  ];

  meta = {
    description = "A bot posting hnbot news";
    homepage = "https://github.com/Mic92/mastodon-hnbot";
    license = lib.licenses.mit;
    mainProgram = "hnbot";
  };
}
