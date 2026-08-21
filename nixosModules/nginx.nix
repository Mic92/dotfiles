{ lib, ... }:
let
  # Every retiolum peer gets its addresses from these ranges (kartei hands out
  # 10.243.0.0/16 and 42::/16 to the tinc interface).
  retiolumRanges = [
    "42::/16"
    "10.243.0.0/16"
  ];

  # allow/deny is inherited by locations that don't define rules of their own,
  # so the server block covers the whole vhost, including the acme challenge
  # (our retiolum CA validates from inside the mesh anyway).
  retiolumOnly = ''
    ${lib.concatMapStringsSep "\n" (range: "allow ${range};") retiolumRanges}
    deny all;
  '';
in
{
  # ".r" names only resolve inside retiolum, but nginx still serves them to
  # anyone on the internet who sends the matching SNI/Host header. Restrict
  # vhosts that are retiolum-only to the vpn. Vhosts mixing ".r" with public
  # names are left alone: denying those would take the public names down too.
  options.services.nginx.virtualHosts = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.submodule (
        { name, config, ... }:
        let
          serverName = if config.serverName != null then config.serverName else name;
          serverNames = [ serverName ] ++ config.serverAliases;
          retiolumHost = lib.all (lib.hasSuffix ".r") serverNames;
        in
        {
          config.extraConfig = lib.mkIf retiolumHost (lib.mkAfter retiolumOnly);
        }
      )
    );
  };
}
