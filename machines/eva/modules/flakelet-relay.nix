{ ... }:
{
  imports = [ ../../../nixosModules/flakelet-relay ];

  services.flakelet-relay = {
    acmeHost = "eva.thalheim.io";
    domain = "eva.thalheim.io";
  };
  services.nginx.virtualHosts."eva.thalheim.io".enableACME = true;
}
