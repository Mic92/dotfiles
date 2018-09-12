{ config, lib, args, ... }: 
with lib;

let
  toHex = let
    intToHex = [
      "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
      "a" "b" "c" "d" "e" "f"
    ];
    toHex' = q: a:
      if q > 0
      then (toHex'
        (q / 16)
        ((elemAt intToHex (mod q 16)) + a))
      else a;
    in v: toHex' v "";
in {
  options = {
    name = mkOption {
      type = types.str;
      default = config._module.args.name;
    };
    id = mkOption {
      type = types.int;
      description = ''
        ID that is used to calculate public/private ip addresses of the container.
      '';
    };
    ipv6 = mkOption {
      type = types.str;
      default = "2a03:4000:13:31e:1::${toHex config.id}";
      description = ''
        Public ipv6 address
      '';
    };
    natIpv4 = mkOption {
      type = types.str;
      default = "100.64.0.${toString config.id}";
      description = ''
        Internal ipv6 address
      '';
    };
    internalIpv6 = mkOption {
      type = types.str;
      default = "fd42:4992:6a6d::${toHex config.id}";
      description = ''
        Internal ipv6 address
      '';
    };
    internalIpv4 = mkOption {
      type = types.str;
      default = "172.23.75.${toString config.id}";
      description = ''
        Internal ipv6 address
      '';
    };
    type = mkOption {
      type = types.enum [ "lxc" "nspawn" ];
      default = "nspawn";
      description = ''
        If it is a legacy lxc containers or a nixos nspawn container
      '';
    };
  };
}
