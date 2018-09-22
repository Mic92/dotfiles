{ lib, ... }:

with builtins;
let
  network = (import ../network.nix) {inherit lib;};

  json = (fromJSON (readFile ../lxc/container.json)).network;
  containers = lib.mapAttrs (name: container:
    container // {
      inherit name;
      ip = container.ipv4;
      ip6 = container.ipv6;
  }) json;

  getServices = container:
    if container ? "services" then
      map (name:
        let
          service = container.services.${name};
          allow = service.allow or [];
        in {
          inherit name;
          ip  = lib.removeSuffix "/32" container.ipv4;
          ip6 = lib.removeSuffix "/128" container.ipv6;
          ula = lib.removeSuffix "/128" container.ula;
          port = toString service.port;
          proto = service.proto or "tcp";
          forward = service.forward or false;
          forward_port = if (isBool (service.forward or false)) then
            toString service.port
          else
            toString service.forward;
          allow = filter (s: s != "all") allow;
          allow_all = elem "all" allow;
        }) (attrNames container.services)
    else
      [];

  services = foldl' (a: name:
    (getServices containers.${name}) ++ a
  ) [] (attrNames containers);

  public_services = filter (s: s.allow_all) services;

  forward_services = filter (s: isInt(s.forward) || isString(s.forward) || s.forward) services;

  forward_jumps = map (name: containers.${name})
    (lib.unique (lib.flatten (catAttrs "allow" services)));

  forward_chains = map (container: {
    container_name = container.name;
    services = filter (srv: elem container.name srv.allow) services;
  }) forward_jumps;

  optionalPort = p: optionalString (p != "") ":${p}";

in {
  networking.nftables.enable = true;
  networking.nftables.ruleset = with containers; ''
    table inet filter {
      chain input {
        type filter hook input priority 0;
        ct state established,related accept
        iif lo accept
        # ssh
        tcp dport {22, 22022} accept
        # Mosh
        udp dport 60000-60010 accept
        # xrdp
        tcp dport 3389 accept

        # iperf
        tcp dport 5001 accept
        # iperf2
        tcp dport 5201 accept
        tcp dport 21 accept

        # influxdb
        tcp dport 8086 accept

        # prosody
        tcp dport 5222 accept # xmpp-client
        tcp dport 5269 accept # xmpp-server
        tcp dport 5280 accept # xmpp-bosh
        tcp dport 5281 accept # bosh-ssl
        tcp dport 6555 accept # xmpp-proxy65

        iifname "${network.bridge}" accept

        meta nfproto ipv6 ip6 nexthdr icmpv6 accept
        meta nfproto ipv4 ip protocol icmp accept

        counter log group 2 prefix "forward"
        meta nfproto ipv4 reject with tcp reset
        meta nfproto ipv6 reject with tcp reset
        reject with icmp type port-unreachable
        reject with icmpv6 type port-unreachable
      }
      chain output {
        type filter hook output priority 0;
      }

      chain forward {
        type filter hook forward priority 0;
        ct state established,related accept
        meta nfproto ipv6 ip6 nexthdr icmpv6 accept
        meta nfproto ipv4 ip protocol icmp accept

        iifname "${network.bridge}" jump lxc_all
        iifname "vc-*" oifname "${network.wan}" accept
        iifname "docker0" oifname "${network.wan}" accept

        ## Allow for all
        ${lib.concatMapStrings (srv: ''
          ip daddr ${srv.ip} ${srv.proto} dport ${srv.port} accept comment "${srv.name}"
          ip6 daddr {${srv.ip6}, ${srv.ula}} ${srv.proto} dport ${srv.port} accept comment "${srv.name}"
        '') forward_services}

        ## LXC targets
        ${lib.concatMapStrings (container: ''
          ip saddr ${container.ip} jump lxc_${container.name}
          ip6 saddr { ${container.ip6}, ${container.ula} } jump lxc_${container.name}
        '') forward_jumps}

        ip6 daddr ${dn42.ip6} ip6 nexthdr 50 accept comment "ipsec"

        counter log group 1 prefix "forward"
        meta nfproto ipv4 reject with tcp reset
        meta nfproto ipv6 reject with tcp reset
        reject with icmp type port-unreachable
        reject with icmpv6 type port-unreachable
      }

      chain lxc_all {
        oifname "lxc_dn42" accept
        oifname "${network.wan}" accept

        ${lib.concatMapStrings (srv: ''
          ip daddr ${srv.ip} ${srv.proto} dport ${srv.port} accept comment "${srv.name}"
          ip6 daddr {${srv.ip6}, ${srv.ula}} ${srv.proto} dport ${srv.port} accept comment "${srv.name}"
        '') public_services}

        ip saddr ${mail.ip} tcp dport smtp accept
        ip6 saddr {${mail.ip6}, ${mail.ula}} tcp dport smtp accept
        tcp dport smtp reject with tcp reset
      }

      ${lib.concatMapStrings (e: ''
        chain lxc_${e.container_name} {
          ${lib.concatMapStrings (srv: ''
            ip daddr ${srv.ip} ${srv.proto} dport ${srv.port} accept comment "${srv.name}"
            ip6 daddr {${srv.ip6}, ${srv.ula}} ${srv.proto} dport ${srv.port} accept comment "${srv.name}"
          '') e.services}
        }
      '') forward_chains}
    }

    table ip6 nat {
      chain prerouting {
        type nat hook prerouting priority 0;
        ip6 daddr ${network.ipv6} iifname "${network.wan}" jump lxc_chain

        # git.higgsboson.tk points to web
        # therefore DNAT port ssh back to git
        ip6 daddr ${web.ip6} tcp dport ssh dnat ${git.ip6} comment "web to git dnat"
      }
      chain lxc_chain {
        ${lib.concatMapStrings (srv: ''
          ${srv.proto} dport ${srv.forward_port} dnat ${srv.ip6} :${srv.port} comment "${srv.name}"
        '') forward_services}
      }
    }

    table ip nat {
      chain prerouting {
        type nat hook prerouting priority 0;
        iifname "${network.wan}" jump lxc_chain

        iifname "${network.bridge}" ip daddr ${network.ipv4} meta mark set 0x42 jump lxc_chain comment "NAT reflection"

        # git.higgsboson.tk points to web
        # therefore DNAT port ssh back to git
        ip daddr ${web.ip} tcp dport ssh dnat ${git.ip} comment "web to git dnat"
      }
      chain postrouting {
        type nat hook postrouting priority 0;

        meta mark 0x42 snat ${network.ipv4} comment "NAT reflection"

        oifname "${network.wan}" masquerade
      }
      chain lxc_chain {
        ${lib.concatMapStrings (srv: ''
        ${srv.proto} dport ${srv.forward_port} dnat ${srv.ip} :${srv.port} comment "${srv.name}"
        '') forward_services}
      }
    }
  '';
}
