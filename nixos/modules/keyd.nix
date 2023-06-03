{ ... }: {
  services.keyd = {
    enable = true;
    settings = {
      main.capslock = "enter";
      # Does not work?
      #main.backspace = "overload(alt,delete)";
    };
  };
}
