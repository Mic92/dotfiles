{ pkgs, ... }: {
  # for pactl
  environment.systemPackages = with pkgs; [ pulseaudio ];

  hardware.pulseaudio.enable = false;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
    # use the example session manager
    media-session.enable = true;
  };
  security.rtkit.enable = true;
}
