let
  outputs = builtins.getFlake (../.);
in {
  turingmachine = outputs.nixosConfigurations.turingmachine.config.system.build.toplevel
  home-manager-desktop = outputs.hmConfigurations.desktop.activate;
  home-manager-common = outputs.hmConfigurations.common.activate;
}
