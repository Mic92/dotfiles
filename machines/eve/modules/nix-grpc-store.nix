{
  config,
  self,
  pkgs,
  ...
}:
let
  # Trust client certs issued by step-ca's OIDC provisioner (see step-ca/default.nix).
  stepCaBundle = pkgs.writeText "step-ca-bundle.pem" ''
    ${config.clan.core.vars.generators.step-ca.files."ca.crt".value}
    ${config.clan.core.vars.generators.step-intermediate-cert.files."intermediate.crt".value}
  '';
in
{
  imports = [ self.inputs.nix-grpc-store.nixosModules.server ];

  # Client cert via OAuth:
  #   step ca certificate joerg@thalheim.io nix-client.crt nix-client.key \
  #     --ca-url https://ca.r --root <(curl -s https://ca.r/ca.crt) --provisioner authelia
  #   nix store info --store 'grpcs://eve.thalheim.io:50051?tls-cert=nix-client.crt&tls-key=nix-client.key'
  services.nix-grpc-daemon = {
    enable = true;
    package = self.inputs.nix-grpc-store.packages.${pkgs.stdenv.hostPlatform.system}.default;
    listen = "0.0.0.0:50051";
    tls = {
      certFile = "/var/lib/acme/thalheim.io/fullchain.pem";
      keyFile = "/var/lib/acme/thalheim.io/key.pem";
      clientCaFile = stepCaBundle;
    };
  };

  # ACME certs are group-readable by nginx
  users.users.nix-grpc-daemon.extraGroups = [ "nginx" ];

  # allowed-users is restricted on servers; the proxy also needs to be
  # trusted so remote builds can import unsigned paths from clients.
  nix.settings.trusted-users = [ "nix-grpc-daemon" ];

  networking.firewall.allowedTCPPorts = [ 50051 ];
}
