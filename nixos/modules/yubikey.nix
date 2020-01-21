{ pkgs, ... }: {

  environment.systemPackages = with pkgs; [
    gnupg
    yubikey-personalization
    yubioath-desktop
  ];

  services.pcscd.enable = true;

  services.udev.packages = with pkgs; [
    yubikey-personalization
  ];
}
