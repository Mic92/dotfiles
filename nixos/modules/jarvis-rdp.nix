{ config, pkgs, ... }: {
  sops.secrets.jarvis-rdp-password = {
    sopsFile = ../secrets/desktop.yaml;
    group = "users";
    mode = "0440";
  };
  environment.systemPackages = [
    (pkgs.runCommand "jarvis" {} ''
      mkdir -p $out/bin $out/share/applications

      cat > $out/bin/jarvis <<EOF
      #!${pkgs.runtimeShell} -e
      exec ${pkgs.freerdp}/bin/wlfreerdp /ipv6 /v:jarvis.r:3389 /u:joerg /p:$(<${config.sops.secrets.jarvis-rdp-password.path}) /cert:tofu "$@"
      EOF
      chmod +x $out/bin/jarvis

      cat > $out/share/applications/jarvis.desktop <<EOF
      [Desktop Entry]
      StartupNotify=true
      Type=Application
      Name=Jarvis
      Exec=jarvis
      EOF
    '')
  ];
}
