{ ... }:
{
  services.uptermd = {
    enable = true;
    openFirewall = true;
    port = 2323;
    extraFlags = [
      "--hostname"
      "upterm.thalheim.io"
    ];
  };

  services.nginx.virtualHosts."upterm.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/".root = ./uptermd;
  };
}
