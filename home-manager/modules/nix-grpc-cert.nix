{ pkgs, ... }:
{
  home.packages = [
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
        echo "Store URI: grpcs://eve.thalheim.io:50051?tls-cert=$dir/client.crt&tls-key=$dir/client.key"
      '';
    })
  ];
}
