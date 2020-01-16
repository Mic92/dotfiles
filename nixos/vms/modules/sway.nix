{ pkgs, ... }: {
  programs.sway = {
    enable = true;
    extraPackages = with pkgs; [
      xwayland
      i3status
      i3status-rust
    ];
    extraSessionCommands = ''
      export SDL_VIDEODRIVER=wayland
      export QT_QPA_PLATFORM=wayland
      export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
      export _JAVA_AWT_WM_NONREPARENTING=1
    '';
  };
}
