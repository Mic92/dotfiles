{ writeCommand, lib }:
let
  source = lib.evalSource [{
    dotfiles.file = {
      path = toString ./..;
      useChecksum = true;
    };
  }];

  deployDotfiles = { user ? "joerg", configuration ? "common" }: targetPath: ''
    sudo -u ${user} zsh <<'EOF'
    cd $HOME
    source $HOME/.zshrc
    homeshick pull
    homeshick symlink
    homeshick cd dotfiles
    nix build --out-link $HOME/.hm-activate ".#hmConfigurations.${configuration}.activation-script"
    $HOME/.hm-activate/activate
    EOF
  '';
in
{
  "joerg@turingmachine" = writeCommand "/bin/joerg-turingmachine" {
    inherit source;
    command = deployDotfiles { configuration = "desktop"; };
    target = turingmachine;
  };

  "joerg@eve" = writeCommand "/bin/joerg-eve" {
    inherit source;
    command = deployDotfiles { configuration = "eve"; };
    target = eve;
  };
  #pkgs.krops.writeDeploy "deploy" {
  #  source = lib.evalSource [
  #    (defaultSources // { nixpkgs.file = nixpkgs.file;})
  #  ];
  #  buildTarget = "joerg@localhost";
  #  crossDeploy = true;
  #  target = "root@matchbox.r";
  #}
}
