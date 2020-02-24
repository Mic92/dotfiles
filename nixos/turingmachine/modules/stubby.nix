{ ... }: {
  services.stubby = {
    enable = true;
    upstreamServers = ''
      # Compute the tls_pubkey_pinset:
      # $ openssl rsa -in /var/lib/acme/thalheim.io/key.pem -outform der -pubout | openssl dgst -sha256 -binary | openssl enc -base64
      - address_data: 95.216.112.61
        tls_auth_name: "dns.thalheim.io"
        tls_pubkey_pinset:
          # rsa
          - digest: "sha256"
            value: 47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=
          # ec384
          - digest: "sha256"
            value: /tYNz0qFGTguihrWOjVfTdhzz4hPW6FaUTjJWdyDBV0=
      - address_data: 2a01:4f9:2b:1605::1
        tls_auth_name: "dns.thalheim.io"
        tls_pubkey_pinset:
          # rsa
          - digest: "sha256"
            value: 47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=
          # ec384
          - digest: "sha256"
            value: /tYNz0qFGTguihrWOjVfTdhzz4hPW6FaUTjJWdyDBV0=
    '';
  };

  networking.nameservers = [ "127.0.0.1" ];
}
