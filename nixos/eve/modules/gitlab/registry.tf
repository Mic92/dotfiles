resource "null_resource" "registry-login" {
  provisioner "local-exec" {
    command = <<-EOT
set -eux -o pipefail
echo ${data.sops_file.secrets.data["GITLAB_TOKEN"]} | podman login registry.gitlab.com -u Mic92 --password-stdin
echo ${data.sops_file.secrets.data["GITHUB_TOKEN"]} | podman login ghcr.io -u Mic92 --password-stdin
echo ${data.sops_file.secrets.data["GITLAB_TOKEN"]} | sudo podman login registry.gitlab.com -u Mic92 --password-stdin
echo ${data.sops_file.secrets.data["GITHUB_TOKEN"]} | sudo podman login ghcr.io -u Mic92 --password-stdin
EOT
  }
}
