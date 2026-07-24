{
  config,
  self,
  pkgs,
  ...
}:
let
  # Client certificates are issued by the local step-ca (via its OIDC
  # provisioner, see step-ca/default.nix), so trust its root + intermediate.
  stepCaBundle = pkgs.writeText "step-ca-bundle.pem" ''
    ${config.clan.core.vars.generators.step-ca.files."ca.crt".value}
    ${config.clan.core.vars.generators.step-intermediate-cert.files."intermediate.crt".value}
  '';
in
{
  imports = [ self.inputs.nix-grpc-store.nixosModules.server ];

  # mTLS-protected gRPC nix-daemon proxy (grpc:// store protocol).
  #
  # Get a client certificate on your laptop via OAuth (Authelia) from step-ca:
  #   step ca certificate joerg@thalheim.io nix-client.crt nix-client.key \
  #     --ca-url https://ca.r --root <(curl -s https://ca.r/ca.crt) \
  #     --provisioner authelia
  # Then:
  #   nix store info --store 'grpcs://eve.thalheim.io:50051?tls-cert=nix-client.crt&tls-key=nix-client.key'
  services.nix-grpc-daemon = {
    enable = true;
    package = self.inputs.nix-grpc-store.packages.${pkgs.stdenv.hostPlatform.system}.default;
    listen = "0.0.0.0:50051";
    tls = {
      # Server side uses the public Let's Encrypt SAN certificate.
      certFile = "/var/lib/acme/thalheim.io/fullchain.pem";
      keyFile = "/var/lib/acme/thalheim.io/key.pem";
      clientCaFile = stepCaBundle;
    };
  };

  # ACME certs on eve are readable by the nginx group.
  users.users.nix-grpc-daemon.extraGroups = [ "nginx" ];

  networking.firewall.allowedTCPPorts = [ 50051 ];
}
