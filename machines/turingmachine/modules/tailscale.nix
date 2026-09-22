# numtide tailnet: shipyard kubectl, grafana.tailf37d1.ts.net
{
  services.tailscale = {
    enable = true;
    openFirewall = true;
    # No routes or DNS takeover by default, only reach *.ts.net.
    extraUpFlags = [ "--accept-dns=false" ];
  };
  networking.firewall.trustedInterfaces = [ "tailscale0" ];
}
