module "dotfiles" {
  source           = "../modules/github-push-bot"
  repo_name        = "Mic92/dotfiles"
  github_token     = data.sops_file.secrets.data["GITHUB_TOKEN"]
  bot_github_token = data.sops_file.secrets.data["buildbot-token"]
}
module "sops-nix" {
  source           = "../modules/github-push-bot"
  repo_name        = "Mic92/sops-nix"
  github_token     = data.sops_file.secrets.data["GITHUB_TOKEN"]
  bot_github_token = data.sops_file.secrets.data["sops-nix-bot-token"]
}
module "infra" {
  source           = "../modules/github-push-bot"
  repo_name        = "nix-community/infra"
  github_token     = data.sops_file.secrets.data["GITHUB_TOKEN"]
  bot_github_token = data.sops_file.secrets.data["nix-infra-bot-token"]
}
module "nix-direnv" {
  source           = "../modules/github-push-bot"
  repo_name        = "nix-community/nix-direnv"
  github_token     = data.sops_file.secrets.data["GITHUB_TOKEN"]
  bot_github_token = data.sops_file.secrets.data["nix-direnv-bot-token"]
}
module "nix-eval-jobs" {
  source           = "../modules/github-push-bot"
  repo_name        = "nix-community/nix-eval-jobs"
  github_token     = data.sops_file.secrets.data["GITHUB_TOKEN"]
  bot_github_token = data.sops_file.secrets.data["nix-eval-jobs-bot-token"]
}
module "nixos-images" {
  source           = "../modules/github-push-bot"
  repo_name        = "nix-community/nixos-images"
  github_token     = data.sops_file.secrets.data["GITHUB_TOKEN"]
  bot_github_token = data.sops_file.secrets.data["nix-eval-jobs-bot-token"]
}
module "disko" {
  source           = "../modules/github-push-bot"
  repo_name        = "nix-community/disko"
  github_token     = data.sops_file.secrets.data["GITHUB_TOKEN"]
  bot_github_token = data.sops_file.secrets.data["disko-bot-token"]
}
module "beherbergung" {
  source           = "../modules/github-push-bot"
  repo_name        = "internet4refugees/beherbergung"
  github_token     = data.sops_file.secrets.data["GITHUB_TOKEN"]
  bot_github_token = data.sops_file.secrets.data["beherbergung-bot-token"]
}
module "home-manager" {
  source           = "../modules/github-push-bot"
  repo_name        = "nix-community/home-manager"
  github_token     = data.sops_file.secrets.data["GITHUB_TOKEN"]
  bot_github_token = data.sops_file.secrets.data["home-manager-bot-token"]
}
#module "blended" {
#  source           = "../modules/github-push-bot"
#  repo_name        = "numtide/blended"
#  github_token     = data.sops_file.secrets.data["GITHUB_TOKEN"]
#  bot_github_token = data.sops_file.secrets.data["blended-bot-token"]
#}
#module "bld" {
#  source           = "../modules/github-push-bot"
#  repo_name        = "numtide/bld"
#  github_token     = data.sops_file.secrets.data["GITHUB_TOKEN"]
#  bot_github_token = data.sops_file.secrets.data["blended-bot-token"]
#}

#locals {
#  tokens = [
#    data.sops_file.secrets.data["buildbot-token"],
#    data.sops_file.secrets.data["sops-nix-bot-token"],
#    data.sops_file.secrets.data["nix-direnv-bot-token"],
#    data.sops_file.secrets.data["nix-eval-jobs-bot-token"],
#    data.sops_file.secrets.data["beherbergung-bot-token"],
#    data.sops_file.secrets.data["blended-bot-token"],
#  ]
#}

#resource "null_resource" "more-stars" {
#  count = length(local.tokens)
#  provisioner "local-exec" {
#    command = <<-EOT
#set -eux -o pipefail
#curl \
#  -X PUT \
#  -H "Accept: application/vnd.github+json" \
#  -H "Authorization: Bearer ${nonsensitive(local.tokens[count.index])}" \
#  https://api.github.com/user/starred/41north/go-async
#EOT
#  }
#}
