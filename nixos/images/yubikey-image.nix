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
    name = "yubikey-guide-2020-08-12.html";
    src = pkgs.fetchFromGitHub {
      owner = "drduh";
      repo = "YubiKey-Guide";
      rev = "f7561616a541182554c2e16ec7c05ac1565a61d7";
      sha256 = "sha256-3CUqLde0NxFEQpYIbL0n7oueF7vEQRuz6tYerqPOL7k=";
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
        url = "https://raw.githubusercontent.com/drduh/config/75ec3f35c6977722d4dba17732d526f704f256ff/gpg.conf";
        sha256 = "sha256-LK29P4+ZAvy9ObNGDNBGP/8+MIUY3/Uo4eJtXhwMoE0=";
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
      ${pkgs.xfce.terminal}/bin/xfce4-terminal &
    '';

    desktopManager = {
      xterm.enable = false;
      xfce.enable = true;
    };
  };
}
