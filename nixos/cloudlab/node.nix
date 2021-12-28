{
  imports = [
    ../modules/users.nix
    ../modules/cloudlab.nix
  ];
  sops.defaultSopsFile = ./secrets/secrets.yaml;
  sops.age.keyFile = "/var/lib/sops-nix/key.txt";
}
