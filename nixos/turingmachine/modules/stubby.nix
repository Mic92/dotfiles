{ ... }: {
  services.stubby = {
    enable = true;
    upstreamServers = ''
      - address_data: 95.216.112.61
        tls_auth_name: "dns.thalheim.io"
        tls_pubkey_pinset:
          - digest: "sha256"
            value: U2OuA8/5Yz14yw9OxzamuzP8K7OPeOzm34l0FBhzQm0=
    '';
  };

  networking.nameservers = [ "127.0.0.1" ];
}
