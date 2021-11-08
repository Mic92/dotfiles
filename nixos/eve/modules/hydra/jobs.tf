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

resource "hydra_jobset" "dotfiles-updates" {
  project     = hydra_project.dotfiles.name
  state       = "enabled"
  visible     = true
  name        = "flake-updates"
  type        = "flake"
  description = "master branch"
  flake_uri   = "github:Mic92/dotfiles/update_flake_lock_action"

  check_interval    = 60
  scheduling_shares = 3000
  keep_evaluations  = 3

  email_notifications = true
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

resource "hydra_jobset" "doctor-cluster-config-flake" {
  project     = hydra_project.doctor-cluster-config.name
  state       = "enabled"
  visible     = true
  name        = "update_flake_lock_action"
  type        = "flake"
  description = "update_flake_lock_action branch"
  flake_uri   = "github:Mic92/doctor-cluster-config/update_flake_lock_action"

  check_interval    = 60
  scheduling_shares = 3000
  keep_evaluations  = 3

  email_notifications = true
}
