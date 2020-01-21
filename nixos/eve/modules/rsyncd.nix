{
  services.rsyncd ={
    enable = true;
    modules =  {
      hase = {
        comment = "Public rsync share.";
        path = "/var/lib/hase";
        "read only" = "yes";
      };
    };
  };
  networking.firewall.allowedTCPPorts = [ 873 ];

  services.netdata.portcheck.checks.rsync.port = 873;

  services.icinga2.extraConfig = ''
    apply Service "Rsync v4 (eve)" {
      import "eve-tcp4-service"
      vars.tcp_port = 873
      assign where host.name == "eve.thalheim.io"
    }

    apply Service "Rsync v6 (eve)" {
      import "eve-tcp6-service"
      vars.tcp_port = 873
      assign where host.name == "eve.thalheim.io"
    }
  '';
}
