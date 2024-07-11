{ inputs, config, ... }:
let
  inherit (inputs.clan-core.lib.facts config.clan.core.clanDir) readFact;
in
{
  imports = [ inputs.data-mesher.nixosModules.data-mesher ];
  services.data-mesher = {
    enable = true;
    tld = "d";
    initNetwork = config.clan.core.machineName == "eve";
    ip = config.clan.core.facts.services.zerotier.public.zerotier-ip.value;
    bootstrapPeers = [
      "http://[${readFact "eve" "zerotier-ip"}]:7331"
    ];
    openFirewall = true;
  };
}
