{ lib, ... }:
{
  services.k3s.enable = true;
  services.k3s.docker = lib.mkForce false;
}
