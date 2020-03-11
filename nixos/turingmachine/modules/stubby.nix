{ ... }: {
  services.stubby = {
    enable = true;
    upstreamServers = ''
      - address_data: 95.216.112.61
        tls_auth_name: "dns.thalheim.io"
      - address_data: 2a01:4f9:2b:1605::1
        tls_auth_name: "dns.thalheim.io"
    '';
  };

  networking.nameservers = [ "127.0.0.1" ];

  programs.captive-browser.enable = true;
  programs.captive-browser.interface = "wlan0";
}
