{lib, ...}:

with lib;

let
  network = (import ./network.nix);
  birdCfg = ipv6: let
    allowedSubnets = if ipv6 then
      "fc00::/7+"
    else
      "172.16.0.0/12+, 10.0.0.0/8+";
  in
''
log syslog all;

router id ${network.ip4_prefix};

#protocol babel {
#  import all;
#  import keep filtered;
#  export all;
#  interface "evenet" {
#    type wireless;
#    rxcost 512;
#  };
#}

define OWNIP = ${if ipv6 then ''
  ${network.ula_prefix}::1
'' else ''
  ${network.ip4_prefix}
''};

protocol device {
  scan time 10;
}

protocol kernel {
  scan time 20;
  device routes;

  import none;
  export filter {
    if source = RTS_STATIC then reject;
    krt_prefsrc = OWNIP;
    krt_metric = 2048;
    accept;
  };
};

protocol ospf {
  import filter {
    if net ~ [ ${allowedSubnets} ] then {
      accept;
    }
    reject;
  };
  export filter {
    if net ~ [ ${allowedSubnets} ] then {
      accept;
    }
    reject;
  };

  area 51 {
${optionalString (!ipv6) "
    stubnet 169.254.0.0/16 { summary; hidden yes; };
    stubnet 192.168.0.0/16 { summary; hidden yes; };
    stubnet 10.100.0.0/16 { summary; hidden yes; };
"}
    interface "enp0s25" {
      cost 5;
      hello 5; retransmit 2; wait 10; dead 20;
      check link yes;
    };
    interface "dummy0" { cost 1; };
    interface "wlp3s0" {
      cost 100;
      hello 5; retransmit 2; wait 10; dead 20;
      check link yes;
    };
    interface "wg-eve6" {
      cost 1004;
      hello 5; retransmit 2; wait 10; dead 60;
    };
    interface "wg-*" {
      cost 1000;
      hello 5; retransmit 2; wait 10; dead 60;
    };
    interface "tun0" {
      cost 900;
      type broadcast;
      hello 5; retransmit 2; wait 10; dead 60;
${optionalString (!ipv6) "
      neighbors { 172.23.75.81; };
"}
    };
  };
}
''; in {
  services = {
    bird = {
      enable = true;
      config = birdCfg false;
    };
    bird6 = {
      enable = true;
      config = birdCfg true;
    };
  };
}
