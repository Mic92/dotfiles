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
in
{
  turingmachine = writeCommand "/bin/turingmachine" {
    inherit source command;
    target = lib.mkTarget "root@turingmachine.r";
  };

  eve = writeCommand "/bin/eve" {
    inherit source command;
    target = "root@eve.thalheim.io";
    #target = "root@129.215.90.4";
    #target = "root@eve.i";
  };

  eddie = writeCommand "/bin/eddie" {
    inherit source command;
    target = "root@eddie.r";
    #target = "root@129.215.90.4";
  };

  eva = writeCommand "/bin/eva" {
    inherit source;
    # 1GB goes OOM while deploying
    target = "root@eve.thalheim.io";
    command = targetPath: ''
      export NIX_SSHOPTS="-o ForwardAgent=yes"
      nix shell 'nixpkgs#git' -c \
         nixos-rebuild switch --flake ${targetPath}/dotfiles#eva \
         --build-host localhost \
         --target-host root@eva.thalheim.io
    '';
  };
}
