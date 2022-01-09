{
  networking.retiolum.ipv6 = "42:0:3c46:47e8:f610:15d1:27a3:674b";
  services.tinc.networks.retiolum.extraConfig = ''
    AddressFamily = ipv6
    ConnectTo = jarvis
  '';
}
