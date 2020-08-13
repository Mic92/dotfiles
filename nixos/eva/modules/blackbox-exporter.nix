{ config, lib, pkgs, ... }:

with lib;

let
  modules = {
    http_2xx = {
      prober = "http";
      timeout = "5s";
    };
    https_2xx = {
      prober = "http";
      timeout = "5s";
      http.fail_if_not_ssl = true;
    };
    tcp_v4 = {
      prober = "tcp";
      timeout = "5s";
      tcp.preferred_ip_protocol = "ip4";
    };
    tcp_v6 = {
      prober = "tcp";
      timeout = "5s";
      tcp.preferred_ip_protocol = "ip6";
    };
    icmp_v4 = {
      prober = "icmp";
      timeout = "5s";
      icmp.preferred_ip_protocol = "ip4";
    };
    icmp_v6 = {
      prober = "icmp";
      timeout = "5s";
      icmp.preferred_ip_protocol = "ip6";
    };
  };
in {
  systemd.services.prometheus-blackbox-exporter.serviceConfig.LimitNOFILE = 1024000;
  services.prometheus.exporters.blackbox = {
    enable = true;
    configFile = pkgs.writeText "blackbox-exporter.yaml" (builtins.toJSON { inherit modules; });
  };
}
