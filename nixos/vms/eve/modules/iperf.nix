{ pkgs, ... }: {
  environment.systemPackages = [ pkgs.iperf3 ];

  networking.firewall.allowedTCPPorts = [
    # iperf2
    5201
    # abuse ftp port
    21
  ];
}
