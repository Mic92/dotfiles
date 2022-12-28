{
  sops.defaultSopsFile = ../secrets/secrets.yaml;
  sops.gnupg.home = "/var/src/.sops-secret";
  sops.gnupg.sshKeyPaths = [ ];
}
