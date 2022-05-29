data "sops_file" "secrets" {
  source_file = "${module.path}/../secrets.enc.json"
}
