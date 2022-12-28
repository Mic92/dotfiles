variable "repo_name" {
  type        = string
  description = "Repository name where to invite the bot to."
}
variable "github_token" {
  type        = string
  description = "Github token of the owner."
}

variable "bot_github_token" {
  type        = string
  description = "Github token of the bot."
}

variable "actions_secret_name" {
  type        = string
  default     = "GH_TOKEN_FOR_UPDATES"
  description = "CI secret name to install."
}
