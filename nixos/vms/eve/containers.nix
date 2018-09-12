{ lib, ...}:

with lib;
with builtins;

let

  lxcJson = (fromJSON (readFile ./lxc/container.json));

  #lxcContainers = lib.mapAttrs (name: container: {
  #  inherit name;
  #  ip = container.ipv4;
  #  ip6 = container.ipv6;
  #  enabled = true;
  #} // container) json.network;

  lxcContainers = lib.filterAttrs
    (_: c: (!(hasAttr "lxc" c)) || c.lxc)
    lxcJson.network;

  #ip  = lib.removeSuffix "/32" container.ipv4;
  #ip6 = lib.removeSuffix "/128" container.ipv6;
  #ula = lib.removeSuffix "/128" container.ula;
  #port = toString service.port;
  #proto = service.proto or "tcp";
  #forward = service.forward or false;
  #forward_port = if (isBool (service.forward or false)) then
  #  toString service.port
  #else
  #  toString service.forward;
  #allow = filter (s: s != "all") allow;
  #allow_all = elem "all" allow;
in {

  # nspawn containers
  eve.containers = {
    dns2.id = 24;
  } // (mapAttrs (name: value: {
    id = toInt (head (match ".*([0-9]+)" value.ipv4));
    type = "lxc";
  }) lxcContainers);

  containers = mapAttrs' (file: _:
    nameValuePair (replaceStrings [".nix"] [""] file) {
      config = {...}: {
        imports = [ (./containers + "/${file}") ];
      };
    }) (builtins.readDir ./containers);
}
