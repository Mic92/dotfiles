module "sops-nix" {
  source = "../modules/github-push-bot"
  repo_name = "Mic92/sops-nix"
  github_token = data.sops_file.secrets.data["GITHUB_TOKEN"]
  bot_github_token = data.sops_file.secrets.data["sops-nix-bot-token"]
}
module "nix-direnv" {
  source = "../modules/github-push-bot"
  repo_name = "nix-community/nix-direnv"
  github_token = data.sops_file.secrets.data["GITHUB_TOKEN"]
  bot_github_token = data.sops_file.secrets.data["nix-direnv-bot-token"]
}
module "nix-eval-jobs" {
  source = "../modules/github-push-bot"
  repo_name = "nix-community/nix-eval-jobs"
  github_token = data.sops_file.secrets.data["GITHUB_TOKEN"]
  bot_github_token = data.sops_file.secrets.data["nix-eval-jobs-bot-token"]
}
module "beherbergung" {
  source = "../modules/github-push-bot"
  repo_name = "internet4refugees/beherbergung"
  github_token = data.sops_file.secrets.data["GITHUB_TOKEN"]
  bot_github_token = data.sops_file.secrets.data["beherbergung-bot-token"]
}
module "home-manager" {
  source = "../modules/github-push-bot"
  repo_name = "nix-community/home-manager"
  github_token = data.sops_file.secrets.data["GITHUB_TOKEN"]
  bot_github_token = data.sops_file.secrets.data["home-manager-bot-token"]
}
