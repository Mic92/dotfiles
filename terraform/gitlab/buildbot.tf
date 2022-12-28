locals {
  buildbot_repos = [
    "dotfiles",
    "nixpkgs"
  ]
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
