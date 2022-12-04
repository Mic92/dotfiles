{
  config,
  pkgs,
  ...
}: {
  sops.secrets.rock-rdp-password = {
    sopsFile = ../secrets/desktop.yaml;
    group = "users";
    mode = "0440";
  };
  environment.systemPackages = [
    (pkgs.runCommand "rock" {} ''
      mkdir -p $out/bin $out/share/applications

      cat > $out/bin/rock <<EOF
      #!${pkgs.runtimeShell} -e
      exec ${pkgs.freerdp}/bin/wlfreerdp /ipv6 /v:rock.r:3389 /u:joerg /p:\$(<${config.sops.secrets.rock-rdp-password.path}) /cert:tofu "\$@"
      EOF
      chmod +x $out/bin/rock

      cat > $out/share/applications/rock.desktop <<EOF
      [Desktop Entry]
      StartupNotify=true
      Type=Application
      Name=Rock
      Exec=rock
      EOF
    '')
  ];
}
