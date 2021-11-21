terraform {
  required_providers {
    gitlab = {
      source = "gitlabhq/gitlab"
      version = "~> 3.4.0"
    }
    sops = {
      source = "carlpett/sops"
      version = "~> 0.5"
    }
  }
}

data "sops_file" "secret" {
  source_file = "secret.enc.json"
}

provider "gitlab" {
  token = var.gitlab_token
}
