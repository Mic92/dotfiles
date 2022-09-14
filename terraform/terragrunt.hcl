terraform {
  before_hook "reset old terraform state" {
    commands = ["init"]
    execute  = ["rm", "-f", ".terraform.lock.hcl"]
  }
  extra_arguments "backend_auth" {
    commands = [
      "init"
    ]

    arguments = [
      "-backend-config=password=${get_env("GITLAB_TOKEN")}"
    ]
  }
}

generate "terraform" {
  path      = "terraform.tf"
  if_exists = "overwrite_terragrunt"
  contents  = <<EOF
terraform {
  backend "http" {
    address        = "https://gitlab.com/api/v4/projects/31493944/terraform/state/${path_relative_to_include()}"
    lock_address   = "https://gitlab.com/api/v4/projects/31493944/terraform/state/${path_relative_to_include()}/lock"
    unlock_address = "https://gitlab.com/api/v4/projects/31493944/terraform/state/${path_relative_to_include()}/lock"
    username       = "Mic92"
    lock_method    = "POST"
    unlock_method  = "DELETE"
    retry_wait_min = "5"
  }


  required_providers {
    digitalocean = { source = "digitalocean/digitalocean" }
    github       = { source = "integrations/github" }
    sops         = { source  = "carlpett/sops" }
    gitlab       = { source  = "gitlabhq/gitlab" }
  }
}
EOF
}
