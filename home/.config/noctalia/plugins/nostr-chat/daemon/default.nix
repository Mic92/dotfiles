{ buildGoModule }:

buildGoModule {
  pname = "nostr-chatd";
  version = "0.1.0";
  src = ./.;
  vendorHash = "sha256-XqMkpulQFwSnCdnlSAKEFQWLKYYwUJ/FFE7Bw56OekI=";

  # modernc.org/sqlite is pure Go — no CGO, no system deps.
  env.CGO_ENABLED = "0";

  meta = {
    description = "Nostr NIP-17 DM bridge for a noctalia-shell chat panel";
    mainProgram = "nostr-chatd";
  };
}
