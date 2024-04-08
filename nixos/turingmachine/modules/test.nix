{ pkgs, self, ... }:
let
  dependencies = [
    pkgs.stdenv.drvPath
    self.nixosConfigurations.your-machine.config.system.build.toplevel
    self.nixosConfigurations.your-machine.config.system.build.diskoScript
  ] ++ builtins.map (i: i.outPath) (builtins.attrValues self.inputs);

  closureInfo = pkgs.closureInfo { rootPaths = dependencies; };
in
# Now add `closureInfo` to your NixOS installer
{
  environment.etc."install-closure".source = "${closureInfo}/store-paths";

  environment.systemPackages = [
    (pkgs.writeShellScriptBin "install-nixos-unattended" ''
      set -eux
      # Replace "/dev/disk/by-id/some-disk-id" with your actual disk ID
      exec ${pkgs.disko}/bin/disko-install --flake "${../../..}#your-machine" --disk vdb "/dev/disk/by-id/some-disk-id"
    '')
  ];

}
