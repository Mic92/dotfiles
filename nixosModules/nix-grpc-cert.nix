{ pkgs, ... }:
{
  environment.systemPackages = [
    # Provision an mTLS client cert for the gRPC nix-daemon on eve via
    # step-ca's Authelia OIDC provisioner (opens a browser to log in).
    (pkgs.writeShellApplication {
      name = "nix-grpc-cert";
      runtimeInputs = [
        pkgs.step-cli
        pkgs.curl
      ];
      text = ''
        dir="''${XDG_DATA_HOME:-$HOME/.local/share}/nix-grpc-store"
        mkdir -p "$dir"
        root="$dir/ca.crt"
        curl -fsS https://ca.r/ca.crt -o "$root"
        step ca certificate "joerg@thalheim.io" "$dir/client.crt" "$dir/client.key" \
          --ca-url https://ca.r --root "$root" --provisioner authelia --force
        echo "Client certificate written to $dir"
        # The grpc store plugin picks up client.crt/client.key from
        # $XDG_DATA_HOME/nix-grpc-store (or /run/nix-grpc-store) by default.
        echo "Store URI: grpc://eve.thalheim.io:50051"

        # Also install to a fixed root-owned location so the nix daemon
        # (remote builder config) can reference stable paths. Use /run so
        # the short-lived cert (24h) doesn't outlive a reboot.
        global=/run/nix-grpc-store
        if sudo -n true 2>/dev/null || sudo -v; then
          sudo install -d -m 0755 "$global"
          sudo install -m 0644 "$dir/client.crt" "$global/client.crt"
          sudo install -m 0600 "$dir/client.key" "$global/client.key"
          echo "Global copy installed to $global (used by the nix daemon by default)"
        else
          echo "sudo unavailable: skipped global copy to $global" >&2
        fi
      '';
    })
  ];
}
