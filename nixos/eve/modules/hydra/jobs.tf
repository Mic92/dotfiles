terraform {
  required_providers {
    hydra = {
      version = "~> 0.1"
      source  = "DeterminateSystems/hydra"
    }
  }
}

provider "hydra" {
  host = "https://hydra.thalheim.io"
  username = "admin"
}

resource "hydra_project" "dotfiles" {
  name         = "dotfiles"
  display_name = "Dotfiles"
  description  = "My NixOS dotfiles"
  homepage     = "https://github.com/Mic92/dotfiles/"
  owner        = "admin"
  enabled      = true
  visible      = true
}

resource "hydra_jobset" "dotfiles-master" {
  project     = hydra_project.dotfiles.name
  state       = "enabled"
  visible     = true
  name        = "master"
  type        = "flake"
  description = "master branch"
  flake_uri   = "github:Mic92/dotfiles"

  check_interval    = 60
  scheduling_shares = 3000
  keep_evaluations  = 3

  email_notifications = true
}

resource "hydra_jobset" "dotfiles-prs" {
  project     = hydra_project.dotfiles.name
  state       = "enabled"
  visible     = true
  name        = "prs"
  type        = "legacy"
  description = "pull requests for dotfiles"

  check_interval    = 60
  scheduling_shares = 3000
  keep_evaluations  = 3

  email_notifications = true

  nix_expression {
    file  = "ci.nix"
    input = "dotfiles"
  }

  input {
    name = "dotfiles"
    type = "git"
    value = "https://github.com/Mic92/dotfiles.git"
    notify_committers = false
  }

  input {
    name = "pr"
    value = "Mic92 dotfiles"
    type = "githubpulls"
    notify_committers = false
  }
}

resource "hydra_project" "doctor-cluster-config" {
  name         = "doctor-cluster-config"
  display_name = "Doctor-cluster"
  description  = " NixOS configuration for TUM cluster"
  homepage     = "https://github.com/Mic92/doctor-cluster-config"
  owner        = "admin"
  enabled      = true
  visible      = true
}

resource "hydra_jobset" "doctor-cluster-config-master" {
  project     = hydra_project.doctor-cluster-config.name
  state       = "enabled"
  visible     = true
  name        = "master"
  type        = "flake"
  description = "master branch"
  flake_uri   = "github:Mic92/doctor-cluster-config"

  check_interval    = 60
  scheduling_shares = 3000
  keep_evaluations  = 3

  email_notifications = true
}
