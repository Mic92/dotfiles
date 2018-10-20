{
  services.openssh = {
    enable = true;
    ports = [
      22022 # legacy
      22
    ];
  };
}
