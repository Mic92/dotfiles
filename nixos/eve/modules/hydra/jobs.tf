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
