{ pkgs, ... }: {
  environment.systemPackages = [ pkgs.iperf3 ];

  # iperf2
  networking.firewall.allowedTCPPorts = [ 5201 ];
  # iperf2
  networking.firewall.allowedUDPPorts = [ 5201 ];
}
