{
  config,
  pkgs,
  ...
}:
{
  sops.secrets.eve-rdp-password = {
    sopsFile = ../secrets/secrets.yaml;
    group = "users";

    mode = "0440";
  };
  sops.secrets.ryan-rdp-password = {
    sopsFile = ../secrets/secrets.yaml;
    group = "users";

    mode = "0440";
  };
  environment.systemPackages = [
    (pkgs.writeShellScriptBin "eve-rdp" ''
      exec ${pkgs.freerdp}/bin/wlfreerdp +clipboard /smart-sizing /multimon -grab-keyboard /ipv6 /v:eve.r:3389 /u:joerg /p:$(<${config.sops.secrets.eve-rdp-password.path}) /cert:tofu "$@"
    '')

    (pkgs.writeShellScriptBin "ryan-rdp" ''
      exec ${pkgs.freerdp}/bin/wlfreerdp +clipboard /smart-sizing /multimon -grab-keyboard /ipv6 /v:ryan.r:3389 /u:joerg /p:$(<${config.sops.secrets.ryan-rdp-password.path}) /cert:tofu "$@"
    '')
  ];
}
