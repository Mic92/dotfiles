# NixOS livesystem to generate yubikeys in an air-gapped manner
# screenshot: https://dl.thalheim.io/wmxIqucOEo2xuLk0Ut45fQ/yubikey-live-system.png
# $ nixos-generator -f iso -c yubikey-image.nix
{ pkgs, ... }: {
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
  ];

  services.udev.packages = with pkgs; [ yubikey-personalization ];
  services.pcscd.enable = true;
  users.extraUsers.root.initialHashedPassword = "";

  # make sure we are air-gapped
  networking.wireless.enable = false;
  networking.dhcpcd.enable = false;

  services.mingetty.helpLine = "The 'root' account has an empty password.";

  services.xserver = {
    enable = true;
    displayManager.auto.enable = true;

    desktopManager = let
      guide = pkgs.stdenv.mkDerivation {
        name = "yubikey-guide-2019-01-21.html";
        src = pkgs.fetchFromGitHub {
          owner = "drduh";
          repo = "YubiKey-Guide";
          rev = "035d98ebbed54a0218ccbf23905054d32f97508e";
          sha256 = "0rzy06a5xgfjpaklxdgrxml24d0vhk78lb577l3z4x7a2p32dbyq";
        };
        buildInputs = [ pkgs.pandoc ];
        installPhase = "pandoc --highlight-style pygments -s --toc README.md -o $out";
      };
    in {
      default = "xfce";
      xterm.enable = false;
      xfce.enable = true;
      xfce.extraSessionCommands = ''
        ${pkgs.midori}/bin/midori ${guide} &
        ${pkgs.xfce.terminal}/bin/xfce4-terminal &
      '';
    };
  };
}
