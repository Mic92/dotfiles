{ config, pkgs, ... }: {
  sops.secrets.jarvis-rdp-password = {
    sopsFile = ../secrets/desktop.yaml;
    group = "users";
    mode = "0440";
  };
  environment.systemPackages = [
    (pkgs.writeShellScriptBin "jarvis-desktop" ''
      exec ${pkgs.freerdp}/bin/wlfreerdp /ipv6 /v:jarvis.r:3389 /u:joerg /p:$(<${config.sops.secrets.jarvis-rdp-password.path}) /cert:tofu "$@"
    '')
  ];
}
