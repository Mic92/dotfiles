{ lib, pkgs, ... }:
let
  network = (import ./network.nix) {inherit lib;};
  containers = network.containers;

  dnsServers = [
  ];

  telegraf-lxc = pkgs.callPackage ../pkgs/telegraf-lxc.nix {};
in {
  security.sudo.extraConfig = ''
    telegraf ALL = NOPASSWD: ${telegraf-lxc}/bin/telegraf-lxc-stats
  '';
  services.telegraf = {
    enable = true;
    extraConfig = {
      global_tags.host = "eve";
      agent.interval = "300s";
      outputs = {
        influxdb = [{
          urls = ["https://influxdb.thalheim.io:8086"];
          database = "eve";
          username = "telegraf";
          user_agent = "telegraf eve";
          password = "Aiph8eedueRoav5a";
          tagpass.host = ["eve"];
        } {
          urls = ["https://influxdb.thalheim.io:8086"];
          database = "rauter";
          username = "telegraf";
          user_agent = "telegraf eve";
          password = "Aiph8eedueRoav5a";
          tagpass.host = ["rauter"];
        }];
      };
      inputs = {
        exec = {
          data_format = "influx";
          commands = [ "/run/wrappers/bin/sudo ${telegraf-lxc}/bin/telegraf-lxc-stats" ];
        };
        cpu = {
          percpu = true;
          totalcpu = true;
          collect_cpu_time = false;
        };
        disk.ignore_fs = ["tmpfs" "devtmpfs"];
        diskio = {};
        kernel = {};
        mem = {};
        processes = {};
        system = {};
        cgroup.paths = [
          "/cgroup/cpu/lxc.slice/*"
          "/cgroup/blkio/lxc.slice/*"
          "/cgroup/memory/lxc.slice/*"
        ];
        http_response = [
          { address = "https://higgsboson.tk"; }
          { address = "https://rss.higgsboson.tk"; }
          { address = "https://cloud.higgsboson.tk/index.php/login"; }
          { address = "https://piwik.higgsboson.tk/"; }
          { address = "https://ist.devkid.net/wiki/Hauptseite"; }
          #{ address = "https://mail.higgsboson.tk"; }
          # { address = "https://cloud.devkid.net"; }
        ];
        dns_query = let
          servers = [
            containers.dns.ipv4
            containers.dns.ula
          ];
          domains = [
            "dns.eve.evenet.dn42"
            "thalheim.io"
            "higgsboson.tk"
            "devkid.net"
            "nek0.eu"
            "nek0.cat"
            "chelnok.de"
            "google.com"
          ];
        in [ {
          inherit servers domains;
          record_type = "A";
        } {
          inherit servers domains;
          record_type = "AAAA";
        } {
          inherit servers;
          domains = [
            "1.75.23.172.in-addr.arpa"
            "1.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.d.6.a.6.2.9.9.4.2.4.d.f.ip6.arpa"
          ];
          record_type = "PTR";
        }
      ];
      netstat = {};
      net.interfaces = ["eth0" "lxc_dn42"];
      nginx.urls = ["https://higgsboson.tk/_status"];
      zfs = {} ;
    };
  };
};
}
