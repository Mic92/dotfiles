{ config
, pkgs
, ...
}: {
  imports = [
    ../../modules/pipewire.nix
    ../../modules/xrdp.nix
  ];
  environment.systemPackages = with pkgs; [
    pavucontrol # for the microphone array
    git # for mycroft
  ];
  sops.secrets.xrdp-password-hash.neededForUsers = true;
  users.users.joerg.passwordFile = config.sops.secrets.xrdp-password-hash.path;
}
