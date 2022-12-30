{
  services.unbound = {
    enable = true;
    settings = {
      server = {
        prefetch = "yes";
        prefetch-key = true;
      };
    };
  };
}
