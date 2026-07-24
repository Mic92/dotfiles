{
  config,
  self,
  pkgs,
  ...
}:
{
  imports = [ self.inputs.nix-grpc-store.nixosModules.server ];

  # mTLS-protected gRPC nix-daemon proxy (grpc:// store protocol).
  # Clients need the client cert/key from the nix-grpc-store vars generator:
  #   nix store info --store 'grpcs://eve.thalheim.io:50051?tls-cert=client-cert&tls-key=client-key&tls-ca=ca-cert'
  clan.core.vars.generators.nix-grpc-store = {
    files = {
      ca-cert.owner = "nix-grpc-daemon";
      ca-key.secret = true;
      server-cert.owner = "nix-grpc-daemon";
      server-key.owner = "nix-grpc-daemon";
      client-cert = { };
      client-key.secret = true;
    };

    runtimeInputs = [ pkgs.openssl ];

    script = ''
      # CA
      openssl ecparam -genkey -name prime256v1 -out "$out/ca-key"
      openssl req -new -x509 -days 3650 -key "$out/ca-key" -out "$out/ca-cert" \
        -subj "/CN=nix-grpc-store eve CA"

      # Server certificate with SAN for eve.thalheim.io
      openssl ecparam -genkey -name prime256v1 -out "$out/server-key"
      openssl req -new -key "$out/server-key" -out "/tmp/server.csr" \
        -subj "/CN=eve.thalheim.io"
      openssl x509 -req -in "/tmp/server.csr" \
        -CA "$out/ca-cert" -CAkey "$out/ca-key" -CAcreateserial \
        -extfile <(echo "subjectAltName=DNS:eve.thalheim.io,DNS:eve.r") \
        -out "$out/server-cert" -days 3650

      # Client certificate
      openssl ecparam -genkey -name prime256v1 -out "$out/client-key"
      openssl req -new -key "$out/client-key" -out "/tmp/client.csr" \
        -subj "/CN=nix-grpc-client"
      openssl x509 -req -in "/tmp/client.csr" \
        -CA "$out/ca-cert" -CAkey "$out/ca-key" -CAcreateserial \
        -out "$out/client-cert" -days 3650
    '';
  };

  services.nix-grpc-daemon = {
    enable = true;
    package = self.inputs.nix-grpc-store.packages.${pkgs.stdenv.hostPlatform.system}.default;
    listen = "0.0.0.0:50051";
    tls = {
      certFile = config.clan.core.vars.generators.nix-grpc-store.files.server-cert.path;
      keyFile = config.clan.core.vars.generators.nix-grpc-store.files.server-key.path;
      clientCaFile = config.clan.core.vars.generators.nix-grpc-store.files.ca-cert.path;
    };
  };

  networking.firewall.allowedTCPPorts = [ 50051 ];
}
