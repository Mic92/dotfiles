terraform {
  backend "http" {
    address        = "https://gitlab.com/api/v4/projects/31493944/terraform/state/github-bots"
    lock_address   = "https://gitlab.com/api/v4/projects/31493944/terraform/state/github-bots/lock"
    unlock_address = "https://gitlab.com/api/v4/projects/31493944/terraform/state/github-bots/lock"
    username       = "Mic92"
    lock_method    = "POST"
    unlock_method  = "DELETE"
    retry_wait_min = "5"
  }
}
