{
  config,
  lib,
  pkgs,
  ...
}: {
  services.uptermd = {
    enable = true;
    openFirewall = true;
    port = 2323;
  };
}
