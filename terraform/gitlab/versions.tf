terraform {
  required_providers {
    github = {
      source  = "integrations/github"
      version = "~> 6.0"
    }
    gitlab = {
      source = "gitlabhq/gitlab"
    }
    sops = {
      source  = "carlpett/sops"
      version = "~> 1.0"
    }
    null = {
      source  = "hashicorp/null"
      version = "~> 3.0"
    }
  }
}
