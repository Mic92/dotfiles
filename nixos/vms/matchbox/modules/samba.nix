{ pkgs, ... }: {
  services.samba.enable = true;
  services.samba.package = pkgs.sambaFull;
  services.samba.extraConfig = ''
    workgroup = WORKGROUP
    netbios name = MATCHBOX
    server string = Raspberry Pi
    #hosts allow = 192.168.1. 127.0.0.1 169.254. 172.23.75.
    interfaces = eth*
    map to guest = Bad User
    max log size = 50
    dns proxy = no
    load printers = yes
    printing = cups
    printcap name = cups
    security = user
    log level = 3

    [printers]
      comment = All Printers
      path = /var/spool/samba
      public = yes
      browseable = yes
      # to allow user 'guest account' to print.
      guest ok = yes
      writable = no
      printable = yes
      create mode = 0700

    [public]
      comment = Netzlaufwerk
      path = /mnt/hdd/public
      public = yes
      only guest = yes
      create mask = 0644
      directory mask = 2777
      writable = yes
      printable = no
  '';

  networking.firewall.allowedTCPPorts = [ 139 445 ];
  networking.firewall.allowedUDPPorts = [ 631 ];
}
