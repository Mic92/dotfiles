{ ... }: {
  services.keyd = {
    enable = true;
    settings = {
      main.capslock = "enter";
    };
  };
}
