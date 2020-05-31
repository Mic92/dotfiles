with import ../krops.nix {
  name = "eve";
};

pkgs.krops.writeCommand "deploy-home-assistant" {
  source = lib.evalSource [{
    inherit dotfiles nixos-config secrets;
    nixpkgs.git = nixpkgs.git;
  }];
  target = "root@eve.thalheim.io";
  command = targetPath: ''
    NIX_PATH=${lib.escapeShellArg targetPath} \
    set -x
    $(${pkgs.nix}/bin/nix-build --show-trace --no-out-link \
      -E '(with import <nixpkgs/nixos> {};
           pkgs.writeScript "update-hass-config"
             config.systemd.services.home-assistant.preStart)')
    systemctl reload home-assistant
  '';
}
