with import <nixpkgs> {};

{
  allowUnfree = true;
  pulseaudio = true;
  chromium = {
    enablePepperFlash = true;
    enablePepperPDF = true;
  };
  packageOverrides = pkgs: with pkgs; {
    home-manager = import ./home-manager { inherit pkgs; };

    staging = buildEnv {
      name = "staging";
      paths = [ ];
    };
  };
}
