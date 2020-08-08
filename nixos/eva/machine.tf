provider "aws" {
  version = "~> 3.0"
  region  = "eu-central-1"
}

resource "aws_key_pair" "mic92" {
  key_name   = "mic92-key"
  public_key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC7ymgN0OyRio14LNeyZg0I3frGXscYg/6Foab+BEE/uvY3iQj4//GZUd60Kp/mbBBWWoMq08Y7SV3wnkMWkhP5qtvTf44b7qNqo6lWyg8IooeqKl8TyNTwbAMxyBRDYl4Czu5IWpMdM8m1QD6G9NKAWWNf+36d4A5THfvUsYUMRpcOaTQchPbtrKdBE5z9No7jscBfmtF0VRxB/OvFImcF+PH2rWmKul99tLX9e6HunInoe1met1z18jThkeByWhSFypYL8JxXR8zLfDB1pT6/3XW062h7a/5qjUvREpMtHHlszuksZJkeU2BcHqQhbQ5BwPXTICjJuAYpeFDg+/Pqg4M0a0icDc+WmoPpsSU7Xx5O5CRvkH66R/lxYh95eF1wEU4K0/Z4m4V3X6BattV0zgu691it4ZidVEBdNtgdFu29pEExaKDcUF689MujE1PREpl/yOx+KiKD7iFJgWKhg2i47oz0s7BNbMwcU7nJJvoBLlePqLWkMsuF+MwwUEolTd21uqWsYzcqB9AkT9xBp11wgWB8+FAi9vWzg5O/A7FXdQ3eV7ZLgJH1MxR4DxtKErBCHzBzs6U+OpiXScp2AYD3OPgffCM2DtiJbcLMQqktNTsTsiw+EgOdwffufmenXcSU6d74KNlu12hsFU8LHyKb9edhHPvFEkdnCuZYyw=="
}

resource "aws_instance" "eva" {
  # https://nixos.org/download.html
  # NixOS-20.03.1554.94e39623a49-x86_64-linux
  ami = "ami-0a1a94722dcbff94c"
  instance_type = "t2.micro"
  key_name = "mic92-key"
  ipv6_address_count = 1
  subnet_id = aws_subnet.eva.id
  vpc_security_group_ids = [ aws_security_group.eva.id ]
  depends_on = [ aws_internet_gateway.eva ]
  root_block_device {
    volume_type = "gp2"
    volume_size = 30
  }
}

resource "aws_vpc" "eva" {
  enable_dns_support = true
  enable_dns_hostnames = true
  assign_generated_ipv6_cidr_block = true
  cidr_block = "172.68.0.0/16"
}

resource "aws_subnet" "eva" {
  vpc_id = aws_vpc.eva.id
  cidr_block = cidrsubnet(aws_vpc.eva.cidr_block, 4, 1)
  ipv6_cidr_block = cidrsubnet(aws_vpc.eva.ipv6_cidr_block, 8, 1)
  map_public_ip_on_launch = true
  assign_ipv6_address_on_creation = true
}

resource "aws_internet_gateway" "eva" {
  vpc_id = aws_vpc.eva.id
}

resource "aws_default_route_table" "eva" {
  default_route_table_id = aws_vpc.eva.default_route_table_id
  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.eva.id
  }

  route {
    ipv6_cidr_block = "::/0"
    gateway_id = aws_internet_gateway.eva.id
  }
}

resource "aws_route_table_association" "eva" {
  subnet_id = aws_subnet.eva.id
  route_table_id = aws_default_route_table.eva.id
}

resource "aws_security_group" "eva" {
  vpc_id = aws_vpc.eva.id

  ingress {
    from_port = 0
    to_port = 0
    protocol = "-1"
    cidr_blocks = ["0.0.0.0/0"]
    ipv6_cidr_blocks = ["::/0"]
  }

  egress {
    from_port = 0
    to_port = 0
    protocol = "-1"
    cidr_blocks = ["0.0.0.0/0"]
    ipv6_cidr_blocks = ["::/0"]
  }
}

output "eva-id" {
  value = aws_instance.eva.id
}

output "eva-ipv4" {
  value = aws_instance.eva.public_ip
}

output "eva-ipv6" {
  value = [ aws_instance.eva.ipv6_addresses ]
}
