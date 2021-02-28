{
  sops.defaultSopsFile = ../secrets/secrets.yaml;
  sops.gnupgHome = "/var/src/.sops-secret";
  sops.sshKeyPaths = [ ];
}
