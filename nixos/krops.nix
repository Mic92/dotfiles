{ writeCommand, lib }:
let
  source = lib.evalSource [{
    dotfiles.file = {
      path = toString ./..;
      useChecksum = true;
    };
  }];
  # sometimes services fail to switch on the first run, but are fine on the second
  command = targetPath: ''
    nixos-rebuild switch --flake ${targetPath}/dotfiles || \
      nixos-rebuild switch --flake ${targetPath}/dotfiles
  '';

  deployDotfiles = user: targetPath: ''
    sudo -u ${user} zsh -c 'cd $HOME && source $HOME/.zshrc && homeshick pull && homeshick symlink && home-manager switch'
  '';
  turingmachine = lib.mkTarget "root@turingmachine.r";
  eve = lib.mkTarget "root@eve.thalheim.io";
  #eve = lib.mkTarget "root@129.215.90.4";
  #eve = lib.mkTarget "root@eve.i";
  eddie = lib.mkTarget "root@eddie.r";
  # eddie = "root@129.215.90.4";
in
{
  turingmachine = writeCommand "/bin/turingmachine" {
    inherit source command;
    target = turingmachine;
  };

  "joerg@turingmachine" = writeCommand "/bin/joerg-turingmachine" {
    inherit source;
    command = deployDotfiles "joerg";
    target = turingmachine;
  };

  eve = writeCommand "/bin/eve" {
    inherit source command;
    target = "root@eve.thalheim.io";
  };

  "joerg@eve" = writeCommand "/bin/joerg-eve" {
    inherit source;
    command = deployDotfiles "joerg";
    target = eve;
  };

  eddie = writeCommand "/bin/eddie" {
    inherit source command;
    target = "root@eddie.r";
  };

  "joerg@eddie" = writeCommand "/bin/joerg-eddie" {
    inherit source;
    command = deployDotfiles "joerg";
    target = eddie;
  };

  eva = writeCommand "/bin/eva" {
    inherit source;
    # 1GB goes OOM while deploying
    target = "root@eve.thalheim.io";
    command = targetPath: ''
      nix shell 'nixpkgs#git' -c \
         nixos-rebuild switch --flake ${targetPath}/dotfiles#eva \
         --build-host localhost \
         --target-host root@eva.thalheim.io
    '';
  };
}
