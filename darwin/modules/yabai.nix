{
  services.yabai.enable = true;
  services.skhd.enable = true;
  services.skhd.skhdConfig = ''
    # focus next window in stack
    alt - j : yabai -m window --focus stack.next
    # focus previous window in stack
    alt - k : yabai -m window --focus stack.prev
  '';
}
