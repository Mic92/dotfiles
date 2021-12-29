{ config, ... }: {
  imports = [
    ../modules/users.nix
    ../modules/cloudlab.nix
  ];
  sops.defaultSopsFile = ./secrets/secrets.yaml;
  services.openssh.hostKeys = [
    { path = config.sops.secrets.ssh-key.path; type = "ed25519"; }
  ];

  sops.secrets.ssh-key = {};
  sops.age.keyFile = "/var/lib/sops-nix/key.txt";
}
