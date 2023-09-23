{
  services.keyd = {
    enable = true;
    keyboards.default.settings = {
      main.capslock = "enter";
      # Does not work?
      #main.backspace = "overload(alt,delete)";
    };
  };
  # seems to break my keyboard after an upgrade
  systemd.services.keyd.restartIfChanged = false;
}
