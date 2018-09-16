# Run ansible on all container

$ cd /etc/nixos/ansible
$ sudo nix-shell --command "ansible-playbook -i /etc/nixos/ansible/inventory /etc/nixos/ansible/site.yml"
