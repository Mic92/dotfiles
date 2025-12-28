{
  config,
  pkgs,
  ...
}:
{
  # SSH key generation via clan vars
  clan.core.vars.generators.radicle = {
    files.ssh-private-key = {
      secret = true;
      owner = "radicle";
    };
    files.ssh-public-key = {
      secret = false;
    };
    runtimeInputs = with pkgs; [ openssh ];
    script = ''
      ssh-keygen -t ed25519 -N "" -f $out/ssh-private-key -C "radicle@${config.networking.hostName}"
      ssh-keygen -y -f $out/ssh-private-key > $out/ssh-public-key
    '';
  };

  # Radicle node (no httpd/webui)
  services.radicle = {
    enable = true;
    privateKeyFile = config.clan.core.vars.generators.radicle.files.ssh-private-key.path;
    publicKey = builtins.readFile config.clan.core.vars.generators.radicle.files.ssh-public-key.path;

    node = {
      openFirewall = true;
      listenAddress = "[::]";
      listenPort = 8776;
    };

    settings = {
      preferredSeeds = [
        "z6MkrLMMsiPWUcNPHcRajuMi9mDfYckSoJyPwwnknocNYPm7@seed.radicle.xyz:8776"
        "z6Mkmqogy2qEM2ummccUthFEaaHvyYmYBYh3dbe9W4ebScxo@iris.radicle.xyz:8776"
      ];
      node = {
        alias = config.networking.hostName;
        seedingPolicy = {
          default = "allow";
          scope = "followed";
        };
        # Auto-follow your DID to accept all your repos
        follow = [
          "did:key:z6MkjE3BSJn4Y129rhqi5rViSUru8KSBcCQdQcDZq1cnjumw"
        ];
        # Peer with other personal nodes
        connect = [
          # eve
          "z6MktZckvzz29eJtUQ4u9bkNu8jihg1sRvknUZMm1xq2stn9@radicle.thalheim.io:8776"
          # eva
          "z6MkwQTGzGVFjmT54Ustr82rc3bMGkjSjeCXQWgSvNNvVnwa@eva.thalheim.io:8776"
          # blob64
          "z6Mkkmnifhqr7bJ48tKjE3KRXKwH9SSwMavNPfsphCpeT94W@blob64.x:8776"
        ];
      };
    };
  };
}
