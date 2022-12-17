# Requires the following module in your flake to be included in your nixos configuration
# { _module.args.self = self; }
{ self, ... }:
# ## Examples
# Get a development shell for the currently running kernel i.e. to hack on a kernel module
# $ nix develop "$(realpath /run/booted-system/flake)#nixosConfigurations.turingmachine.config.boot.kernelPackages.kernel"
# $ tar -xvf $src
# $ cd linux-*
# $ zcat /proc/config.gz  > .config
# $ make scripts prepare modules_prepare
# $ make -C . M=drivers/block/null_blk
{
  system.extraSystemBuilderCmds = ''
    ln -s ${self} $out/flake
  '';
}
