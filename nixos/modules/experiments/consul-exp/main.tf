resource "vault_mount" "consul" {
  path = "consul"
  type = "consul"
}

resource "vault_generic_secret" "consul" {
  path = "consul/config/access"
  depends_on = [
    vault_mount.consul
  ]
  disable_read = true
  data_json = jsonencode({
    address = "127.0.0.1:8500"
  })
}

resource "vault_consul_secret_backend_role" "admin" {
  depends_on = [
    vault_generic_secret.consul
  ]
  name    = "admin"
  backend = "consul"
  ttl     = 600

  policies = ["global-management"]

  # BUG global-management is detected as "consul_roles" when reading
  lifecycle {
    ignore_changes = [
      policies,
      consul_roles
    ]
  }
}


data "vault_generic_secret" "admin" {
  depends_on = [
    vault_consul_secret_backend_role.admin
  ]
  path = "consul/creds/admin"
}

provider "consul" {
  token = data.vault_generic_secret.admin.data.token
}

resource "consul_keys" "foo" {
  token = data.vault_generic_secret.admin.data.token
  key {
    path  = "foo"
    value = "bar"
  }
}

#data "consul_acl_policy" "management" {
#  name = "global-management"
#}
#
#resource "consul_acl_token" "vault" {
#  description = "ACL token for Consul secrets engine in Vault"
#  policies    = [data.consul_acl_policy.management.name]
#  local       = true
#}
#
#
#data "consul_acl_token_secret_id" "vault" {
#  accessor_id = consul_acl_token.vault.id
#}

#resource "vault_consul_secret_backend" "consul" {
#  path                      = "consul"
#  description               = "Manages the Consul backend"
#  address                   = "consul:8500"
#  token                     = data.consul_acl_token_secret_id.vault.secret_id
#  default_lease_ttl_seconds = 3600
#  max_lease_ttl_seconds     = 3600
#}
