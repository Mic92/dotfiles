terraform {
  required_providers {
    gitlab = {
      source  = "gitlabhq/gitlab"
      version = "~> 3.8.0"
    }
    github = {
      source  = "integrations/github"
      version = "~> 4.18.0"
    }
    sops = {
      source  = "carlpett/sops"
      version = "~> 0.5"
    }
  }
  backend "http" {}
}

data "sops_file" "secrets" {
  source_file = "secrets.enc.json"
}

provider "github" {
  #token = data.sops_file.secrets.data["GITHUB_TOKEN"]
}

provider "gitlab" {
  token = data.sops_file.secrets.data["GITLAB_TOKEN"]
}

data "github_repositories" "my-repos" {
  query = "user:Mic92"
}

resource "gitlab_project" "repos" {
  for_each               = toset(data.github_repositories.my-repos.full_names)
  name                   = element(split("/", each.key), 1)
  import_url             = "https://github.com/${each.key}"
  mirror                 = true
  shared_runners_enabled = false
  ci_config_path = lookup({
    }, each.key,
    # sane default
    ".gitlab-ci.yml:Mic92/dotfiles")
  visibility_level = "public"
}

resource "gitlab_project_variable" "cachix-key" {
  for_each = toset([
    gitlab_project.repos["Mic92/dotfiles"].id,
    gitlab_project.repos["Mic92/doctor-cluster-config"].id
  ])
  project   = each.key
  key       = "CACHIX_SIGNING_KEY"
  value     = data.sops_file.secrets.data["CACHIX_SIGNING_KEY"]
  protected = true
  masked    = true
}

resource "gitlab_project_variable" "github-token" {
  for_each = toset([
    gitlab_project.repos["Mic92/dotfiles"].id,
    gitlab_project.repos["Mic92/doctor-cluster-config"].id
  ])
  project   = each.key
  key       = "GITHUB_TOKEN"
  value     = data.sops_file.secrets.data["GITHUB_TOKEN"]
  protected = true
  masked    = true
}
