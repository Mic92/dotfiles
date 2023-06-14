variable "tsig_key" {}

provider "digitalocean" {}
provider "dns" {
  update {
    server        = "ns1.thalheim.io"
    key_name      = "bbc."
    key_algorithm = "hmac-sha256"
    key_secret    = var.tsig_key
  }
}

resource "digitalocean_droplet" "bbc" {
  name   = "bbc"
  region = "lon1"
  ipv6   = true
  image  = "ubuntu-22-04-x64"
  size   = "s-1vcpu-1gb"
  ssh_keys = [
    36184046,
    36184041
  ]
  provisioner "local-exec" {
    command = "ssh-keygen -R bbc.lekwati.com"
  }
}

resource "dns_a_record_set" "bbc" {
  zone = "bbc.lekwati.com."
  addresses = [
    digitalocean_droplet.bbc.ipv4_address
  ]
  ttl = 300
}

resource "dns_aaaa_record_set" "bbc" {
  zone = "bbc.lekwati.com."
  addresses = [
    digitalocean_droplet.bbc.ipv6_address
  ]
  ttl = 300
}

output "instruction" {
  value = <<EOT
You can now login by typing:

  sshuttle -r root@bbc.lekwati.com 0/0 ::/0

To stop run

  1. Press Ctrl-C
  2. Type 'terragrunt destroy'
EOT
}
