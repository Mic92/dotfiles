{
  imports = [
    ../../modules/retiolum.nix
  ];

  networking.retiolum = {
    ipv4 = "10.243.29.174";
    ipv6 = "42:0:3c46:70c7:8526:2adf:7451:8bbb";
  };
}
