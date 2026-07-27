# Roaming-friendly transport for herdr --remote: exec channels over
# WebSocket that survive network drops, authenticated via Authelia OIDC.
# https://github.com/Mic92/herdr-eternal-server
{ self, ... }:
{
  imports = [ self.inputs.herdr-eternal.nixosModules.default ];

  services.herdr-eternal-server = {
    enable = true;
    user = "joerg";
    oidc = {
      issuer = "https://auth.thalheim.io";
      clientId = "herdr-eternal";
      # Authelia issues opaque per-client subject identifiers; this is the
      # one it minted for the joerg account on the herdr-eternal client.
      allowedSub = "1967320f-21d8-4f96-a7ef-21a08c0b24bb";
    };
    nginx = {
      enable = true;
      hostName = "herdr.thalheim.io";
    };
    # Direct QUIC path (UDP 7443) so roaming clients keep their connection
    # across address changes; TLS from the wildcard ACME cert.
    quic = {
      enable = true;
      useACMEHost = "thalheim.io";
    };
  };

  services.nginx.virtualHosts."herdr.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
  };
}
