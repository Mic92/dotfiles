{ lib, ... }:

{
  networking.nftables.enable = true;
  networking.nftables.ruleset = ''
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

        # postfix
        tcp dport 25 accept # smtp
        tcp dport 465 accept # stmps
        tcp dport 587 accept # submission

        # dovecot
        tcp dport 143 accept # imap
        tcp dport 993 accept # imaps
        tcp dport 4190 accept # sieve

        # bind
        tcp dport 53 accept
        udp dport 53 accept

        # nginx
        tcp dport 80 accept # http
        tcp dport 443 accept # https

        # weechat
        tcp dport 4242 accept
        # irc-dcc
        tcp dport 6760-6769 accept

        # syncthing
        tcp dport 22000 accept
        udp dport 21027 accept

        # squid
        tcp dport 8888 accept
        tcp dport 8889 accept

        # teamspeak
        tcp dport 30033 accept # ts3_ft
        tcp dport 10011 accept # ts3_sq
        tcp dport 41144 accept # ts3_dns
        udp dport 9987 accept # ts3_devkid
        udp dport 22222 accept # ts3_martijn
        udp dport 5037 accept # ts3_martin
        udp dport 9000 accept # ts3_putzy

        # tinc
        tcp dport 655 accept
        udp dport 655 accept

        # netdata
        ip saddr { 10.243.29.0/12 } tcp dport 19999 accept
        ip6 saddr { 42:4992:6a6d::/16 } tcp dport 19999 accept

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

        iifname "docker0" oifname "eth0" accept

        counter log group 1 prefix "forward"
        meta nfproto ipv4 reject with tcp reset
        meta nfproto ipv6 reject with tcp reset
        reject with icmp type port-unreachable
        reject with icmpv6 type port-unreachable
      }
    }

    table ip6 nat {
      chain prerouting {
        type nat hook prerouting priority 0;
      }
    }

    table ip nat {
      chain prerouting {
        type nat hook prerouting priority 0;
      }
      chain postrouting {
        type nat hook postrouting priority 0;
      }
    }
  '';
}
