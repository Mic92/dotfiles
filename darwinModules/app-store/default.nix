{ pkgs, ... }:
let
  apps = [
    497799835 # xcode
  ];
in
{
  environment.systemPackages = [ pkgs.mas ];
  system.activationScripts.xcode.text = ''
    echo "Syncing apps from the App Store..."
    ${pkgs.python3.interpreter} ${./declarative-app-store.py} ${builtins.toString apps}
  '';
}
