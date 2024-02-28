# NixOS livesystem to generate yubikeys in an air-gapped manner
# screenshot: https://dl.thalheim.io/ZF5Y0yyVRZ_2MWqX2J42Gg/2020-08-12_16-00.png
# $ nixos-generate -f iso -c yubikey-image.nix
#
# to test it in a vm:
#
# $ nixos-generate --run -f vm -c yubikey-image.nix
{ pkgs, ... }:
let
  guide = pkgs.stdenv.mkDerivation {
    name = "yubikey-guide-2024-02-12.html";
    src = pkgs.fetchFromGitHub {
      owner = "drduh";
      repo = "YubiKey-Guide";
      rev = "53ed405";
      sha256 = "sha256-dY8MFYJ9WSnvcfa8d1a3gNt52No7eN8aacky1zwJpbI=";
    };
    buildInputs = [ pkgs.pandoc ];
    installPhase = ''
      pandoc --highlight-style pygments -s --toc README.md | \
        sed -e 's/<keyid>/\&lt;keyid\&gt;/g' > $out
    '';
  };
in
{
  environment.interactiveShellInit = ''
    export GNUPGHOME=/run/user/$(id -u)/gnupghome
    if [ ! -d $GNUPGHOME ]; then
      mkdir $GNUPGHOME
    fi
    cp ${
      pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/drduh/config/944faed/gpg.conf";
        sha256 = "sha256-3oTHeGZ9nGJ+g+lnRSEcyifNca+V9SlpjBV1VNvrnNU=";
      }
    } "$GNUPGHOME/gpg.conf"
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

  services.getty.helpLine = "The 'root' account has an empty password.";

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
      ${pkgs.xfce.xfce4-terminal}/bin/xfce4-terminal &
    '';

    desktopManager = {
      xterm.enable = false;
      xfce.enable = true;
    };
  };
}
