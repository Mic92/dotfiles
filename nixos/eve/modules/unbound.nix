{
  imports = [
    ../../modules/unbound.nix
  ];
  # don't conflict with unbound
  services.unbound.settings.server.interface = [ "::1" "127.0.0.1" "192.168.53.53" ];
  # no need for a systemd-resolved stub
  services.resolved.enable = false;
}
