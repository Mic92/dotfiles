terraform {
  required_providers {
    gitlab = {
      source  = "gitlabhq/gitlab"
      version = "~> 3.8.0"
    }
    github = {
      source  = "integrations/github"
      version = "~> 4.19.0"
    }
    sops = {
      source  = "carlpett/sops"
      version = "~> 0.5"
    }
  }
}

provider "github" {
  token = data.sops_file.secrets.data["GITHUB_TOKEN"]
}

provider "gitlab" {
  token = data.sops_file.secrets.data["GITLAB_TOKEN"]
}
