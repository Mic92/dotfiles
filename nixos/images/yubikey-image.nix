# NixOS livesystem to generate yubikeys in an air-gapped manner
# screenshot: https://dl.thalheim.io/wmxIqucOEo2xuLk0Ut45fQ/yubikey-live-system.png
# $ nixos-generate -f iso -c yubikey-image.nix
{ pkgs, ... }:

let
  guide = pkgs.stdenv.mkDerivation {
    name = "yubikey-guide-2020-08-12.html";
    src = pkgs.fetchFromGitHub {
      owner = "drduh";
      repo = "YubiKey-Guide";
      rev = "f7561616a541182554c2e16ec7c05ac1565a61d7";
      sha256 = "sha256-3CUqLde0NxFEQpYIbL0n7oueF7vEQRuz6tYerqPOL7k=";
    };
    buildInputs = [ pkgs.pandoc ];
    installPhase = "pandoc --highlight-style pygments -s --toc README.md -o $out";
  };
in {
  environment.interactiveShellInit = ''
    export GNUPGHOME=/run/user/$(id -u)/gnupghome
    if [ ! -d $GNUPGHOME ]; then
      mkdir $GNUPGHOME
    fi
    cp ${pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/drduh/config/662c16404eef04f506a6a208f1253fee2f4895d9/gpg.conf";
      sha256 = "118fmrsn28fz629y7wwwcx7r1wfn59h3mqz1snyhf8b5yh0sb8la";
    }} "$GNUPGHOME/gpg.conf"
    echo "\$GNUPGHOME has been set up for you. Generated keys will be in $GNUPGHOME."
  '';

  environment.systemPackages = with pkgs; [
    yubikey-personalization
    cryptsetup
    pwgen
    midori
    paperkey
    gnupg
    ctmg
  ];

  services.udev.packages = with pkgs; [ yubikey-personalization ];
  services.pcscd.enable = true;

  # make sure we are air-gapped
  networking.wireless.enable = false;
  networking.dhcpcd.enable = false;

  services.mingetty.helpLine = "The 'root' account has an empty password.";

  security.sudo.wheelNeedsPassword = false;
  users.users.yubikey = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    shell = "/run/current-system/sw/bin/bash";
  };

  services.xserver = {
    enable = true;
    displayManager.autoLogin.enable = true;
    displayManager.autoLogin.user = "yubikey";
    displayManager.defaultSession = "xfce";
    displayManager.sessionCommands = ''
      ${pkgs.midori}/bin/midori ${guide} &
      ${pkgs.xfce.terminal}/bin/xfce4-terminal &
    '';

    desktopManager = {
      xterm.enable = false;
      xfce.enable = true;
    };
  };
}
