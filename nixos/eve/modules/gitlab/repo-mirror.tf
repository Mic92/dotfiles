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
}

data "sops_file" "secrets" {
  source_file = "secrets.enc.json"
}

provider "github" {
  token = data.sops_file.secrets.data["GITHUB_TOKEN"]
}

provider "gitlab" {
  token = data.sops_file.secrets.data["GITLAB_TOKEN"]
}

data "github_repositories" "my-repos" {
  query = "user:Mic92 is:public"
}

data "github_repositories" "my-non-archived-repos" {
  query = "user:Mic92 is:public archived:false"
}

resource "gitlab_project" "repos" {
  for_each               = toset(data.github_repositories.my-repos.full_names)
  name                   = element(split("/", each.key), 1)
  import_url             = "https://github.com/${each.key}"
  mirror                 = true
  mirror_trigger_builds  = true
  shared_runners_enabled = false
  ci_config_path = lookup({
    }, each.key,
    # sane default
  ".gitlab-ci.yml@Mic92/dotfiles")
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

resource "gitlab_service_github" "github" {
  for_each       = toset(data.github_repositories.my-repos.full_names)
  project        = gitlab_project.repos[each.key].id
  token          = data.sops_file.secrets.data["GITHUB_TOKEN"]
  repository_url = "https://github.com/${each.key}"
}

resource "github_repository_webhook" "gitlab" {
  for_each   = toset(data.github_repositories.my-non-archived-repos.names)
  repository = each.key

  configuration {
    url          = "https://gitlab.com/api/v4/projects/Mic92%2F${each.key}/mirror/pull?private_token=${data.sops_file.secrets.data["GITLAB_TOKEN"]}"
    content_type = "form"
    insecure_ssl = false
  }

  active = true

  events = ["push", "pull_request"]
}

resource "null_resource" "update-runner-token" {
  provisioner "local-exec" {
    command = <<EOT
    val="CI_SERVER_URL=https://gitlab.com\nREGISTRATION_TOKEN=${gitlab_project.repos["Mic92/dotfiles"].runners_token}"
sops \
   --set "[\"gitlab-runner-registration\"] \"$val\"" \
  ../../secrets/secrets.yaml
EOT
  }
}
