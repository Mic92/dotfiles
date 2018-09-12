## USAGE
# $ nix-build kexec-installer.nix
# can be deployed remote like this
# $ rsync -aL -e ssh result/ <host>:
## Customize it like this
# # custom-installer.nix
# (import ./kexec-installer.nix) {
#   extraConfig = {pkgs, ... } {
#     user.extraUsers.root.openssh.authorizedKeys.keys = [ "<your-key>" ];
#     services.openssh = {
#        enable = true;
#        startWhenNeeded = true;
#     }
#   }
# }
# $ nix-build custom-installer.nix
# $ ls -la ./result
{
  extraConfig ? {...}: {},
}:
let
  pkgs = import <nixpkgs> {};
  config = (import <nixpkgs/nixos> {
    configuration = {
      imports = [
        <nixpkgs/nixos/modules/installer/netboot/netboot-minimal.nix>
        extraConfig
      ];
    };
  }).config;

  image = pkgs.runCommand "image" { buildInputs = [ pkgs.nukeReferences ]; } ''
    mkdir $out
    cp ${config.system.build.kernel}/bzImage $out/kernel
    cp ${config.system.build.netbootRamdisk}/initrd $out/initrd
    nuke-refs $out/kernel
  '';

  inherit (config.system) build;
  kexecScript = pkgs.writeScript "kexec-installer" ''
    #!/bin/sh

    cd "''${0%/*}"
    export PATH=.${pkgs.kexectools}/bin:$PATH

    if ! kexec -v >/dev/null 2>&1; then
      echo "kexec not found: please install kexec-tools" 2>&1
      exit 1
    fi
    kexec --load .${image}/kernel \
      --initrd=.${image}/initrd \
      --command-line "init=${builtins.unsafeDiscardStringContext config.system.build.toplevel}/init ${toString config.boot.kernelParams}"

    if systemctl --version >/dev/null 2>&1; then
      systemctl kexec
    else
      kexec -e
    fi
  '';
in pkgs.callPackage <nixpkgs/nixos/lib/make-system-tarball.nix> {
  storeContents = [
    { object = kexecScript; symlink = "/kexec_nixos"; }
  ];
  contents = [];
}

  #in pkgs.linkFarm "netboot" [
  #  { name = "initrd.gz"; path = "${build.netbootRamdisk}/initrd"; }
  #  { name = "bzImage";   path = "${build.kernel}/bzImage"; }
  #  { name = "kexec-installer"; path = kexecScript; }
  #]
