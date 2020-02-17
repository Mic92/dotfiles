{
  services.nginx.virtualHosts."_" = {
    listen = [
      { addr = "127.0.0.1"; port = 80; }
      { addr = "[2a01:4f9:2b:1605::1]"; port = 80; }
    ];
    locations."/stub_status".extraConfig = ''
      stub_status;
      # Security: Only allow access from the IP below.
      allow 127.0.0.1;
      allow ::1;
      # Deny anyone else
      deny all;
    '';
  };
}
