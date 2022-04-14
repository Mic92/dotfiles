{pkgs, ...}: {
  system.activationScripts.diff = ''
    if [[ -e  /run/current-system ]]; then
      ${pkgs.nix}/bin/nix store diff-closures /run/current-system "$systemConfig"
    fi
  '';
}
