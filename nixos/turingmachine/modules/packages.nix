{ pkgs, config, ... }:

{
  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    zfs.enableUnstable = true;
  };

  imports = [ ../../modules/packages.nix ];

  environment.systemPackages = with pkgs; [ cntr ntfs3g ];
}
