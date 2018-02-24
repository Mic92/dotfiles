with import <nixpkgs> {};

{
  allowUnfree = true;
  pulseaudio = true;
  chromium.enablePepperPDF = true;

  packageOverrides = pkgs: with pkgs; {
    stable = import <stable> {};

    staging = buildEnv {
      name = "staging";
      paths = [ ];
    };
  };
}
