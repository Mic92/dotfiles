{
  formats,
  lib,
  writeShellApplication,
  openssh,
  sops,
  pkgs,
}:
let
  nix-uci = pkgs.python3.pkgs.callPackage ./nix-uci.nix { };
in
{
  writeUci =
    configuration:
    let
      res = lib.evalModules {
        modules = [
          {
            _module.args = {
              inherit pkgs;
            };
          }
          ./module-options.nix
          configuration
        ];
      };
      json = (formats.json { }).generate "openwrt-config.json" {
        inherit (res.config) uci openwrt;
      };
    in
    {
      inherit json;
      # Secrets are only decrypted at deploy time and never enter the store.
      deploy = writeShellApplication {
        name = "openwrt-deploy";
        runtimeInputs = [
          nix-uci
          openssh
          sops
        ];
        text = ''
          if [ $# -lt 1 ]; then
            echo "usage: openwrt-deploy root@HOST [ssh options...]" >&2
            exit 1
          fi
          target=$1
          shift
          # render before connecting, so a sops failure leaves the router untouched
          program=$(nix-uci ${json})
          printf '%s\n' "$program" | ssh "$@" "$target" ucode -
        '';
      };
    };
  inherit nix-uci;
}
