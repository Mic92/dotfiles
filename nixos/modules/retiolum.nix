{ config, ... }: {
  services.tinc.networks.retiolum = {
    ed25519PrivateKeyFile = config.sops.secrets.tinc-ed25519.path;
    rsaPrivateKeyFile = config.sops.secrets.tinc-rsa.path;
  };
  sops.secrets.tinc-ed25519 = { };
  sops.secrets.tinc-rsa = { };
}
