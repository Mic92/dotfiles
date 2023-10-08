locals {
  buildbot_repos               = ["dotfiles", "nixpkgs"]
  buildbot_repos_nix_community = ["harmonia", "disko", "dream2nix"]
  buildbot_repos_numtide       = ["nixos-anywhere"]
}

resource "github_repository_webhook" "buildbot" {
  for_each   = toset(local.buildbot_repos)
  repository = each.key

  configuration {
    url = "https://buildbot.thalheim.io/change_hook/github"
    # needs to be kept in sync with eve's secrets
    secret       = data.sops_file.secrets.data["github-webhook-secret"]
    content_type = "form"
    insecure_ssl = false
  }

  active = true

  events = ["push", "pull_request"]
}

provider "github" {
  alias = "nix-community"
  owner = "nix-community"
  token = data.sops_file.secrets.data["GITHUB_TOKEN"]
}

resource "github_repository_webhook" "buildbot-nix-community" {
  for_each   = toset(local.buildbot_repos_nix_community)
  repository = each.key

  configuration {
    url = "https://buildbot.thalheim.io/change_hook/github"
    # needs to be kept in sync with eve's secrets
    secret       = data.sops_file.secrets.data["github-webhook-secret"]
    content_type = "form"
    insecure_ssl = false
  }

  active = true

  events   = ["push", "pull_request"]
  provider = github.nix-community
}

provider "github" {
  alias = "numtide"
  owner = "numtide"
  token = data.sops_file.secrets.data["GITHUB_TOKEN"]
}
resource "github_repository_webhook" "buildbot-numtide" {
  for_each   = toset(local.buildbot_repos_numtide)
  repository = each.key

  configuration {
    url = "https://buildbot.thalheim.io/change_hook/github"
    # needs to be kept in sync with eve's secrets
    secret       = data.sops_file.secrets.data["github-webhook-secret"]
    content_type = "form"
    insecure_ssl = false
  }

  active = true

  events   = ["push", "pull_request"]
  provider = github.numtide
}
