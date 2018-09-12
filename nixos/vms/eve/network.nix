{ lib, ... }:
with builtins;
let
  json = (fromJSON (readFile ./lxc/container.json));
in rec {
  wan = "eth0";
  bridge = "br0";

  zone = json.zone;
  ipv4 = json.zone.a;
  ipv6 = json.zone.aaaa;

  containers = lib.mapAttrs (name: container: {
    inherit name;
    ip = container.ipv4;
    ip6 = container.ipv6;
    enabled = true;
  } // container) json.network;

  lxcContainers = lib.filterAttrs
    (_: c: (!(hasAttr "lxc" c)) || c.lxc)
    containers;
}
