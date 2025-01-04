{ inputs, config, ... }:
let
  inherit (inputs.clan-core.lib.facts config.clan.core.settings.directory) readFact;
in
{
  imports = [ inputs.data-mesher.nixosModules.data-mesher ];
  services.data-mesher = {
    enable = true;
    tld = "d";
    initNetwork = config.clan.core.settings.machine.name == "eve";
    interface = config.clan.core.networking.zerotier.name;
    bootstrapPeers = [ "http://[${readFact "eve" "zerotier-ip"}]:7331" ];
    openFirewall = true;
  };
}
