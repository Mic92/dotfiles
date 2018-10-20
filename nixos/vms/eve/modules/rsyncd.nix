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
}
