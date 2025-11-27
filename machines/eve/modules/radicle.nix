{
  config,
  pkgs,
  lib,
  ...
}:
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
    # Use the actual public key content from the vars file
    publicKey = builtins.readFile config.clan.core.vars.generators.radicle.files.ssh-public-key.path;

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
      };
    };

    # Node settings
    settings = {
      preferredSeeds = [
        "z6MkrLMMsiPWUcNPHcRajuMi9mDfYckSoJyPwwnknocNYPm7@seed.radicle.xyz:8776"
        "z6Mkmqogy2qEM2ummccUthFEaaHvyYmYBYh3dbe9W4ebScxo@seed.radicle.garden:8776"
      ];
      node = {
        seedingPolicy = {
          default = "allow";
          scope = "followed";
        };
        # Auto-follow your DID to accept all your repos
        follow = [
          "did:key:z6MkjE3BSJn4Y129rhqi5rViSUru8KSBcCQdQcDZq1cnjumw"
        ];
      };
      web = {
        pinned = {
          repositories = [
            "rad:z3gpeDzWxqV8iBEN8RcJNZEPVWmJf"
            "rad:z3oWHBpUaHgN9pPapQ1dswRmQpErJ"
          ];
        };
      };
    };
  };

  # Serve radicle-explorer as static files (override upstream's proxy)
  services.nginx.virtualHosts."radicle.thalheim.io" = {
    # Reset listen to use eve's default (with specific IPs) - upstream sets generic ones
    listen = lib.mkForce [
      {
        addr = "[::1]";
        port = 443;
        ssl = true;
      }
      {
        addr = "[42:0:3c46:70c7:8526:2adf:7451:8bbb]";
        port = 80;
      }
      {
        addr = "[42:0:3c46:70c7:8526:2adf:7451:8bbb]";
        port = 443;
        ssl = true;
      }
      {
        addr = "[${config.networking.eve.ipv6.address}]";
        port = 80;
      }
      {
        addr = "[${config.networking.eve.ipv6.address}]";
        port = 443;
        ssl = true;
      }
      {
        addr = "0.0.0.0";
        port = 80;
      }
      {
        addr = "0.0.0.0";
        port = 443;
        ssl = true;
      }
    ];

    # Set root at virtualHost level for static file serving
    root = lib.mkForce "${pkgs.radicle-explorer}";

    # Override "/" to serve static files instead of proxying
    locations."/" = {
      proxyPass = lib.mkForce null;
      tryFiles = "$uri $uri/ /index.html =404";
      extraConfig = ''
        expires 1h;
        add_header Cache-Control "public, immutable";
      '';
    };
    # API endpoints go to radicle-httpd
    locations."/api/" = {
      proxyPass = "http://127.0.0.1:8080";
      recommendedProxySettings = true;
    };
  };
}
