{
  config,
  pkgs,
  ...
}:
{
  # mTLS binary cache for testing nix client certificate authentication
  # See: https://github.com/NixOS/nix/pull/13030
  #
  # Usage with nix (once PR is merged):
  # nix-store --store https://cache2.thalheim.io?tls-certificate=/path/to/client.crt&tls-private-key=/path/to/client.key -r /nix/store/...

  # Generate CA and client certificates using clan vars
  clan.core.vars.generators.mtls-cache = {
    files = {
      # CA certificate and key - nginx needs to read the CA cert
      ca-cert.owner = "nginx";
      ca-key.secret = true;
      # Client certificate and key (for testing)
      client-cert = { };
      client-key.secret = true;
    };

    runtimeInputs = [ pkgs.openssl ];

    script = ''
      # Generate CA key and certificate
      openssl ecparam -genkey -name prime256v1 -out "$out/ca-key"
      openssl req -new -x509 -days 3650 -key "$out/ca-key" -out "$out/ca-cert" \
        -subj "/CN=cache2.thalheim.io CA"

      # Generate client key and certificate
      openssl ecparam -genkey -name prime256v1 -out "$out/client-key"
      openssl req -new -key "$out/client-key" -out /tmp/client.csr \
        -subj "/CN=nix-client"
      openssl x509 -req -in /tmp/client.csr \
        -CA "$out/ca-cert" -CAkey "$out/ca-key" -CAcreateserial \
        -out "$out/client-cert" -days 3650
      rm -f /tmp/client.csr
    '';
  };

  # Nginx virtual host with mTLS
  services.nginx.virtualHosts."cache2.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;

    # mTLS configuration
    extraConfig = ''
      ssl_client_certificate ${config.clan.core.vars.generators.mtls-cache.files.ca-cert.path};
      ssl_verify_client on;
    '';

    # Proxy to harmonia (same backend as cache.thalheim.io)
    locations."/".extraConfig = ''
      proxy_pass http://127.0.0.1:5000;
      proxy_set_header Host $host;
      proxy_redirect http:// https://;
      proxy_http_version 1.1;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header Upgrade $http_upgrade;
      proxy_set_header Connection $connection_upgrade;

      # Pass client certificate info to backend (optional, for logging/debugging)
      proxy_set_header X-SSL-Client-Verify $ssl_client_verify;
      proxy_set_header X-SSL-Client-DN $ssl_client_s_dn;
    '';
  };
}
