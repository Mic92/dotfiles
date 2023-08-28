import http.client
import json
import urllib.request
from pathlib import Path
from typing import Any

from twisted.python import log


class HttpResponse:
    def __init__(self, raw: http.client.HTTPResponse) -> None:
        self.raw = raw

    def json(self) -> Any:
        return json.load(self.raw)

    def headers(self) -> http.client.HTTPMessage:
        return self.raw.headers


def http_request(
    url: str,
    method: str = "GET",
    headers: dict[str, str] = {},
    data: dict[str, Any] | None = None,
) -> HttpResponse:
    body = None
    if data:
        body = json.dumps(data).encode("ascii")
    headers = headers.copy()
    headers["User-Agent"] = "buildbot-nix"
    req = urllib.request.Request(url, headers=headers, method=method, data=body)
    resp = urllib.request.urlopen(req)
    return HttpResponse(resp)


def paginated_github_request(url: str, token: str) -> list[dict[str, Any]]:
    next_url: str | None = url
    repos = []
    while next_url:
        res = http_request(
            next_url,
            headers={"Authorization": f"token {token}"},
        )
        next_url = None
        link = res.headers()["Link"]
        if link is not None:
            links = link.split(", ")
            for link in links:  # pagination
                link_parts = link.split(";")
                if link_parts[1].strip() == 'rel="next"':
                    next_url = link_parts[0][1:-1]
        repos += res.json()
    return repos


class GithubProject:
    def __init__(self, repo: dict[str, Any]) -> None:
        self.repo = repo

    @property
    def name(self) -> str:
        return self.repo["full_name"]

    @property
    def url(self) -> str:
        return self.repo["html_url"]

    @property
    def id(self) -> str:
        n = self.repo["full_name"]
        return n.replace("/", "-")

    @property
    def default_branch(self) -> str:
        return self.repo["default_branch"]

    @property
    def topics(self) -> list[str]:
        return self.repo["topics"]


def load_projects(github_token: str, repo_cache_file: Path) -> list[GithubProject]:
    if repo_cache_file.exists():
        log.msg("fetching github repositories from cache")
        repos: list[dict[str, Any]] = json.loads(repo_cache_file.read_text())
    else:
        log.msg("fetching github repositories from api")
        repos = paginated_github_request(
            "https://api.github.com/user/repos?per_page=100",
            github_token,
        )
        repo_cache_file.write_text(json.dumps(repos, indent=2))

    return [GithubProject(repo) for repo in repos]
