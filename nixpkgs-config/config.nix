with import <nixpkgs> {};

{
  allowUnfree = true;
  pulseaudio = true;
  chromium.enablePepperPDF = true;

  packageOverrides = pkgs: with pkgs; {
    stable = import <stable> {};
    home-manager = import ./home-manager { inherit pkgs; };

    staging = buildEnv {
      name = "staging";
      paths = [ ];
    };
  };
}
