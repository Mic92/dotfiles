# Requires the following module in your flake to be included in your nixos configuration
# { _module.args.self = self; }
{ self, ... }:
# ## Examples
# Get a development shell for the currently running kernel:
# $ nix develop "$(realpath /run/booted-system/flake)#nixosConfigurations.turingmachine.config.boot.kernelPackages.kernel"
{
  system.extraSystemBuilderCmds = ''
    ln -s ${self} $out/flake
  '';
}
