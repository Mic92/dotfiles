{ pkgs, ... }:

{
  services.spotifyd = {
    enable = false;
    settings = {
      global = {
        username_cmd = "${pkgs.rbw}/bin/rbw get spotify --field username";
        password_cmd = "${pkgs.rbw}/bin/rbw get spotify";
        backend = "pulseaudio"; # pipewire provides pulseaudio compatibility
        device_name = "turingmachine";
        bitrate = 320;
        cache_path = "/var/cache/spotifyd";
        no_audio_cache = false;
        volume_normalisation = true;
        normalisation_pregain = -10;
        device_type = "computer";
        zeroconf_port = 5354;
      };
    };
  };

  # Open firewall for Spotify Connect discovery
  networking.firewall.allowedTCPPorts = [ 5354 ];
  networking.firewall.allowedUDPPorts = [ 5354 ];

  # Add Spotify frontends
  environment.systemPackages = with pkgs; [
    ncspot # Terminal UI for Spotify
    spotify # Official Spotify client
  ];
}
