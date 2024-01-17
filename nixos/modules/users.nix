{ config, pkgs, ... }:
let
  keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE joerg@turingmachine"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOBXTForyB6oNMK5bbGpALKU4lPsKRGxNLhrE/PnHHq7 shannan@bernie"
  ];
in
{
  users.users = {
    joerg = {
      isNormalUser = true;
      home = "/home/joerg";
      extraGroups = [ "audio" "wheel" "docker" "plugdev" "vboxusers" "adbusers" "input" "kvm" "wireshark" "dialout" ];
      shell = "/run/current-system/sw/bin/zsh";
      uid = 1000;
      openssh.authorizedKeys.keys = keys;
    };
    root.openssh.authorizedKeys.keys = keys;
  };

  users.extraUsers.root.hashedPasswordFile =
    config.clanCore.secrets.root-password.secrets.root-password-hash.path;
  clanCore.secrets.root-password = {
    secrets.root-password = { };
    secrets.root-password-hash = { };
    generator.path = with pkgs; [ coreutils xkcdpass mkpasswd ];
    generator.script = ''
      xkcdpass -n 3 -d - > $secrets/root-password
      cat $secrets/root-password | mkpasswd -s -m sha-512 > $secrets/root-password-hash
    '';
  };

  sops.secrets."${config.clanCore.machineName}-root-password-hash".neededForUsers = true;

  boot.initrd.network.ssh.authorizedKeys = keys;

  security.sudo.wheelNeedsPassword = false;

  imports = [ ./zsh.nix ];
}
