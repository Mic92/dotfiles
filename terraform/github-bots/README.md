# Manage CI bots in repos

If you use github actions to update dependencies in an automated way, github ci
will not run on pull requests created by those actions. The workaround is to
create a new (bot) github account and invite it to the repository.

Then you can create a github api token in that bot account and add it as an
action secret, e.g.
https://github.com/nix-community/nix-eval-jobs/blob/main/.github/workflows/update-flake-lock.yml#L21

The Terraform code here does not automate the creation of bot github accounts,
but the part where they are invited to the repository. See also
[main.tf](main.tf)
