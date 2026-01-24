{ config, pkgs, ... }:
{
  services.rustdesk-server = {
    enable = true;
    openFirewall = true;
    signal.relayHosts = [ "rustdesk.thalheim.io" ];
  };

  clan.core.vars.generators.rustdesk-server = {
    files.id_ed25519 = { };
    files.id_ed25519_pub.secret = false;
    runtimeInputs = with pkgs; [ openssh ];
    script = ''
      ssh-keygen -t ed25519 -f "$out/id_ed25519" -N "" -C "rustdesk"
      mv "$out/id_ed25519.pub" "$out/id_ed25519_pub"
    '';
  };

  # Link generated keys to rustdesk state directory
  systemd.tmpfiles.rules = [
    "L+ /var/lib/rustdesk/id_ed25519 - - - - ${config.clan.core.vars.generators.rustdesk-server.files.id_ed25519.path}"
    "L+ /var/lib/rustdesk/id_ed25519.pub - - - - ${config.clan.core.vars.generators.rustdesk-server.files.id_ed25519_pub.path}"
  ];
}
