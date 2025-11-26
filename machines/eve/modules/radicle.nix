{ config, pkgs, ... }:
{
  # SSH key generation via clan vars (follows eve's pattern)
  clan.core.vars.generators.radicle = {
    files.ssh-private-key = {
      secret = true;
      owner = "radicle";
    };
    files.ssh-public-key = {
      secret = false;
    };
    migrateFact = "radicle";
    runtimeInputs = with pkgs; [ openssh ];
    script = ''
      ssh-keygen -t ed25519 -N "" -f $out/ssh-private-key -C "radicle@eve"
      ssh-keygen -y -f $out/ssh-private-key > $out/ssh-public-key
    '';
  };

  # Radicle services (upstream module)
  services.radicle = {
    enable = true;
    privateKeyFile = config.clan.core.vars.generators.radicle.files.ssh-private-key.path;
    publicKey = config.clan.core.vars.generators.radicle.files.ssh-public-key.path;

    # Node configuration
    node = {
      openFirewall = true; # Opens port 8776
      listenAddress = "[::]"; # IPv4 and IPv6
      listenPort = 8776;
    };

    # HTTP gateway configuration
    httpd = {
      enable = true;
      nginx = {
        serverName = "radicle.thalheim.io";
        useACMEHost = "thalheim.io"; # Uses existing wildcard cert
        forceSSL = true;
        quic = true; # Override upstream module's false default

        # Additional nginx configuration for radicle-explorer
        locations."/explorer/" = {
          alias = "${pkgs.radicle-explorer}/";
          index = "index.html";
          tryFiles = "$uri $uri/ /explorer/index.html";
          extraConfig = ''
            # Cache static assets
            expires 1h;
            add_header Cache-Control "public, immutable";
          '';
        };
      };
    };

    # Optional: Radicle node settings (freeform JSON)
    settings = {
      # The module automatically sets node.alias to serverName
      # and node.externalAddresses when nginx is enabled
      # Additional settings can go here if needed
    };
  };
}
