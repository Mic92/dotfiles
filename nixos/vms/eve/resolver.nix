{ lib, config, ... }: 

with lib;
with builtins;
let
  hints = toFile "container-hosts" ''
    ${lib.concatMapStrings (container: ''
      ${container.internalIpv4} ${container.name}
      ${container.internalIpv6} ${container.name}
    '') (attrValues config.eve.containers)}
  '';
in {
  services.kresd = {
    enable = true;
    extraConfig = ''
      modules = {
        predict = {
           window = 15, -- 15 minutes sampling window
           period = 6*(60/15) -- track last 6 hours
        },
        'policy',
        'view',
        'hints > iterate',
      }
      view:addr('127.0.0.1/8', function (req, qry) return policy.PASS end)
      view:addr('::1/128', function (req, qry) return policy.PASS end)

      view:addr('100.64.0.0/10', function (req, qry) return policy.PASS end)
      view:addr('172.23.75.0/24', function (req, qry) return policy.PASS end)
      view:addr('fd42:4992:6a6d/48', function (req, qry) return policy.PASS end)
      view:addr('2a03:4000:13:31e::/64', function (req, qry) return policy.PASS end)

      view:addr('0.0.0.0/0', function (req, qry) return policy.DROP end)

      hints.add_hosts('${hints}')
    '';
  };
}
