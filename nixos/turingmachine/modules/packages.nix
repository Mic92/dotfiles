{ pkgs, ... }: {
  imports = [ ../../modules/packages.nix ];

  environment.systemPackages = with pkgs; [ cntr ntfs3g ];
}
