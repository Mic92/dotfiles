{ pkgs, ... }:
let
  key = ''
    ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC7ymgN0OyRio14LNeyZg0I3frGXscYg/6Foab+BEE/uvY3iQj4//GZUd60Kp/mbBBWWoMq08Y7SV3wnkMWkhP5qtvTf44b7qNqo6lWyg8IooeqKl8TyNTwbAMxyBRDYl4Czu5IWpMdM8m1QD6G9NKAWWNf+36d4A5THfvUsYUMRpcOaTQchPbtrKdBE5z9No7jscBfmtF0VRxB/OvFImcF+PH2rWmKul99tLX9e6HunInoe1met1z18jThkeByWhSFypYL8JxXR8zLfDB1pT6/3XW062h7a/5qjUvREpMtHHlszuksZJkeU2BcHqQhbQ5BwPXTICjJuAYpeFDg+/Pqg4M0a0icDc+WmoPpsSU7Xx5O5CRvkH66R/lxYh95eF1wEU4K0/Z4m4V3X6BattV0zgu691it4ZidVEBdNtgdFu29pEExaKDcUF689MujE1PREpl/yOx+KiKD7iFJgWKhg2i47oz0s7BNbMwcU7nJJvoBLlePqLWkMsuF+MwwUEolTd21uqWsYzcqB9AkT9xBp11wgWB8+FAi9vWzg5O/A7FXdQ3eV7ZLgJH1MxR4DxtKErBCHzBzs6U+OpiXScp2AYD3OPgffCM2DtiJbcLMQqktNTsTsiw+EgOdwffufmenXcSU6d74KNlu12hsFU8LHyKb9edhHPvFEkdnCuZYyw== joerg@turingmachine
  '';
  legacyKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDGoQXNL3B1+pS4WYfhvn4ULb6oCNovT+dpWist7osToj5UVQ64odlcemnSG07GRcEnwf2zDTYq8eatomGQ94VsnmWuKaYzF8nqNl+qHRM49nS+Myi2ETn0B5fnMSh45lmkjR5rL/tb02EXUVoNf7acE2K3Q8M/tGFEdCdQNuqEgishi5nrs/WvZHn0cxP1anv8WRtm2qlj0jtH1rYmo7n/xsPb15FNBaE92aQXTkGoj6xdQknGWnGjLLm33lGIxRKvHTJ9T2NGte4gTYC/CADPxU2x5nq8zGDTNna/YMUyKmlqgGm+p+sE9dERmxKtquLgyE8mNvjDSMvtnrkMojN5 joerg@turingmachine";
in
{
  nixpkgs.localSystem.system = "aarch64-linux";

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;
  boot.kernelPackages = pkgs.linuxPackages;
  documentation.enable = false;

  boot.kernelParams = [ "cma=32M" ];

  services.journald.extraConfig = ''
    Storage = volatile
    RuntimeMaxFileSize = 10M;
  '';

  swapDevices = [
    { device = "/swapfile"; size = 1024; }
  ];


  fileSystems = {
    "/boot/firmware" = {
      device = "/dev/disk/by-label/FIRMWARE";
      fsType = "vfat";
    };
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
    };
  };
}
