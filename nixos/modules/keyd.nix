{ ... }: {
  services.keyd = {
    enable = true;
    keyboards.default.settings = {
      main.capslock = "enter";
      # Does not work?
      #main.backspace = "overload(alt,delete)";
    };
  };
}
