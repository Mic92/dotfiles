variable "tsig_key" {}

terraform {
  required_providers {
    digitalocean = {
      source = "digitalocean/digitalocean"
    }
  }
}

provider "digitalocean" {}
provider "dns" {
  update {
    server        = "ns2.thalheim.io"
    key_name      = "bbc."
    key_algorithm = "hmac-sha256"
    key_secret    = var.tsig_key
  }
}

resource "digitalocean_droplet" "bbc" {
  name = "bbc"
  region = "lon1"
  ipv6 = true
  image  = "ubuntu-22-04-x64"
  size = "s-1vcpu-1gb"
  ssh_keys = [
    36184046,
    36184041
  ]
}

resource "dns_a_record_set" "bbc" {
  zone = "bbc.lekwati.com."
  name = "@"
  addresses = [
    digitalocean_droplet.bbc.ipv4_address
  ]
  ttl = 300
}

resource "dns_aaaa_record_set" "bbc" {
  zone = "bbc.lekwati.com."
  name = "@"
  addresses = [
    digitalocean_droplet.bbc.ipv6_address
  ]
  ttl = 300
}

output "instruction" {
  value = <<EOT
You can now login by running

  ssh -D 1234 root@bbc.lekwati.com

To stop run

  terraform destroy
EOT
}
