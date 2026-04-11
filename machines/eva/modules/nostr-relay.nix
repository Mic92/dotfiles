# Secondary strfry relay, mirrored with eve.
#
#   wss://nostr2.thalheim.io
#
# Clients are expected to add *both* relay URLs; Nostr handles
# redundancy at the client, not via DNS failover.
#
# The NIP-29 groups relay is intentionally *not* mirrored here: group
# state is authoritative under the relay's own private key, so a second
# instance would be a different authority rather than a replica.
{
  imports = [ ../../../nixosModules/strfry.nix ];

  services.mic92-strfry = {
    enable = true;
    domain = "nostr2.thalheim.io";
    description = "A general-purpose Nostr relay (mirror of nostr.thalheim.io)";
    # eva has no wildcard cert for thalheim.io, so request a per-host
    # cert via HTTP-01 like the other eva vhosts.
    acme.enableACME = true;
    syncPeers = [ "wss://nostr.thalheim.io" ];
  };
}
