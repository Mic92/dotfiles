{ pkgs, ... }:
{
  # for pactl
  environment.systemPackages = with pkgs; [
    pulseaudio
    pamixer
  ];

  services.pulseaudio.enable = false;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
  security.rtkit.enable = true;
}
