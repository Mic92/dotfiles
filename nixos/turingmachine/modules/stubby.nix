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
            value: /zJHMpHhhKuL9bjIbVG1SMsRXmqv0MKh4W+3Sej4J/8=
          # ec384
          - digest: "sha256"
            value: gN2FtyubY3GCUQSiboHFGgy6qTraG61T019sXi9QDLA=
      - address_data: 2a01:4f9:2b:1605::1
        tls_auth_name: "dns.thalheim.io"
        tls_pubkey_pinset:
          # rsa
          - digest: "sha256"
            value: /zJHMpHhhKuL9bjIbVG1SMsRXmqv0MKh4W+3Sej4J/8=
          # ec384
          - digest: "sha256"
            value: gN2FtyubY3GCUQSiboHFGgy6qTraG61T019sXi9QDLA=
    '';
  };

  networking.nameservers = [ "127.0.0.1" ];

  programs.captive-browser.enable = true;
  programs.captive-browser.interface = "wlan0";
}
