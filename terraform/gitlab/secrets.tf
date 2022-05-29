data "sops_file" "secrets" {
  source_file = "${path.module}/../secrets.enc.json"
}
