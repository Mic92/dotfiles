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
        echo "Store URI: grpc://eve.thalheim.io:50051"

        # System-wide copy for the nix daemon (plugin default lookup path).
        global=/var/lib/nix-grpc-store
        run0 sh -c 'install -d -m 0755 "$0" && install -m 0644 "$1/client.crt" "$0/client.crt" && install -m 0600 "$1/client.key" "$0/client.key"' "$global" "$dir"
        echo "Global copy installed to $global"
      '';
    })
  ];
}
