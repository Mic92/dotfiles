{
  imports = [
    ../../modules/retiolum.nix
  ];

  networking.retiolum = {
    ipv4 = "10.243.29.174";
    ipv6 = "42:4992:6a6d:a00::1";
  };
}
