{
  services.teamspeak3.enable = true;

  networking.firewall.allowedTCPPorts = [
    30033 # ts3_ft
    10011 # ts3_sq
  ];

  networking.firewall.allowedUDPPorts = [
    9987 # ts3_devkid
    22222 # ts3_martijn
    5037 # ts3_martin
    9000 # ts3_putzy
  ];
}
