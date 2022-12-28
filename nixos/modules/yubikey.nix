{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    gnupg
    yubikey-personalization
    yubioath-desktop
  ];

  services.udev.extraRules = ''
    # Yubikey 4/5 U2F+CCID
    SUBSYSTEM=="usb", ATTR{idVendor}=="1050", ATTR{idProduct}=="0406", ENV{ID_SECURITY_TOKEN}="1", GROUP="wheel"
  '';

  services.udev.packages = with pkgs; [
    yubikey-personalization
  ];
}
