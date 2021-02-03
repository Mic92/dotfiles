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
    nix-shell -p git --run '
      nixos-rebuild switch --flake ${targetPath}/dotfiles -L --keep-going || \
        nixos-rebuild switch --flake ${targetPath}/dotfiles -L --keep-going
    '
  '';

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
    command = deployDotfiles { configuration = "desktop"; };
    target = turingmachine;
  };

  eve = writeCommand "/bin/eve" {
    inherit source command;
    target = eve;
  };

  "joerg@eve" = writeCommand "/bin/joerg-eve" {
    inherit source;
    command = deployDotfiles { configuration = "eve"; };
    target = eve;
  };

  bernie = writeCommand "/bin/bernie" {
    inherit source command;
    target = "root@bernie.r";
  };

  rock = writeCommand "/bin/rock" {
    inherit source;
    target = "root@eve.thalheim.io";
    command = targetPath: ''
      nixos-rebuild switch --flake ${targetPath}/dotfiles#rock \
        --build-host localhost \
        --target-host root@rock.r
    '';
  };

  matchbox = writeCommand "/bin/matchbox" {
    inherit source;
    target = "root@turingmachine";
    command = targetPath: ''
      nixos-rebuild switch --flake ${targetPath}/dotfiles#matchbox \
        --build-host localhost \
        --target-host root@192.168.178.2
    '';
  };
  #matchbox = writeCommand "/bin/matchbox" {
  #  inherit source command;
  #  target = "root@192.168.178.2";
  #};

  eddie = writeCommand "/bin/eddie" {
    inherit source command;
    target = "root@eddie.r";
  };

  "joerg@eddie" = writeCommand "/bin/joerg-eddie" {
    inherit source;
    command = deployDotfiles { configuration = "desktop"; };
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

  #pkgs.krops.writeDeploy "deploy" {
  #  source = lib.evalSource [
  #    (defaultSources // { nixpkgs.file = nixpkgs.file;})
  #  ];
  #  buildTarget = "joerg@localhost";
  #  crossDeploy = true;
  #  target = "root@matchbox.r";
  #}
}
