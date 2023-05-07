{ ... }: {
  programs.hyprland = {
    enable = true;
  };

  #wayland.windowManager.hyprland = {
  #  enable = true;
  #  xwayland = {
  #    enable = true;
  #    hidpi = true;
  #  };
  #  package = pkgs.hyprland;
  #  extraConfig = ''
  #    #-- Monitors ----------------------------------------------------
  #    # Configure your Display resolution, offset, scale and Monitors here, use `hyprctl monitors` to get the info.
  #    # See: https://wiki.hyprland.org/Configuring/Monitors/

  #    # Startup
  #    exec-once = waybar
  #    exec-once = foot --server
  #    exec-once = firefox
  #    exec-once = signal-desktop --enable-features=UseOzonePlatform --ozone-platform=wayland
  #    exec-once = nm-applet --indicator
  #    exec-once = dunst
  #    exec-once = ferdium
  #    exec-once = kanshi
  #    exec-once = dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP
  #    exec-once = systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP

  #    # Startup / XWayland
  #    # See: https://wiki.hyprland.org/Configuring/XWayland/
  #    exec-once=xprop -root -f _XWAYLAND_GLOBAL_OUTPUT_SCALE 32c -set _XWAYLAND_GLOBAL_OUTPUT_SCALE 2

  #    $ws1 = 1:web
  #    $ws2 = 2:dev
  #    $ws3 = 3:im
  #    $ws4 = 4:signal
  #    $ws5 = 5:doc
  #    $ws6 = 6
  #    $ws7 = 7
  #    $ws8 = 8
  #    $ws9 = 9
  #    $ws0 = 0

  #    monitor=,preferred,auto,1

  #    workspace=,name:$ws1

  #    #-- Input ----------------------------------------------------
  #    # Configure mouse and touchpad here.
  #    input {
  #      kb_layout = us
  #      kb_options = caps:escape,compose:menu
  #      kb_variant = altgr-intl
  #      kb_model =
  #      kb_rules =
  #      follow_mouse = 1
  #      sensitivity = 0
  #      touchpad {
  #        natural_scroll = no
  #      }
  #    }

  #    #-- General ----------------------------------------------------
  #    # General settings like MOD key, Gaps, Colors, etc.
  #    # See: https://wiki.hyprland.org/Configuring/Variables/#general
  #    general {
  #      gaps_in = 5
  #      gaps_out = 5

  #      border_size = 1
  #      no_border_on_floating = false
  #      col.active_border = rgba(33ccffee) rgba(00ff99ee) 45deg
  #      col.inactive_border = rgba(595959aa)

  #      layout = master
  #    }

  #    #-- Decoration ----------------------------------------------------
  #    # Decoration settings like Rounded Corners, Opacity, Blur, etc.
  #    # See: http://wiki.hyprland.org/Configuring/Variables/#decoration
  #    decoration {
  #      rounding = 10
  #      multisample_edges = 0

  #      blur = yes
  #      blur_size = 3
  #      blur_passes = 1
  #      blur_new_optimizations = on

  #      drop_shadow = yes
  #      shadow_range = 4
  #      shadow_render_power = 3
  #      col.shadow = rgba(1a1a1aee)
  #    }

  #    #-- Animations ---------------------------------------------------
  #    # See: https://wiki.hyprland.org/Configuring/Variables/#animations
  #    # See: https://wiki.hyprland.org/Configuring/Animations/
  #    animations {
  #      enabled = yes

  #      bezier = myBezier, 0.05, 0.9, 0.1, 1.05

  #      animation = windows, 1, 7, myBezier
  #      animation = windowsOut, 1, 7, default, popin 80%
  #      animation = border, 1, 10, default
  #      animation = fade, 1, 7, default
  #      animation = workspaces, 1, 6, default
  #    }

  #    #-- Dwindle ----------------------------------------------------
  #    # See: https://wiki.hyprland.org/Configuring/Dwindle-Layout/
  #    dwindle {
  #      pseudotile = yes
  #      preserve_split = yes
  #    }

  #    #-- Master ----------------------------------------------------
  #    # See: https://wiki.hyprland.org/Configuring/Master-Layout/
  #    master {
  #      new_is_master = true
  #    }

  #    gestures {
  #        # See https://wiki.hyprland.org/Configuring/Variables/ for more
  #        workspace_swipe = off
  #    }

  #    # Example per-device config
  #    # See https://wiki.hyprland.org/Configuring/Keywords/#executing for more
  #    device:epic mouse V1 {
  #        sensitivity = -0.5
  #    }

  #    # Example windowrule v1
  #    # windowrule = float, ^(kitty)$
  #    # Example windowrule v2
  #    # windowrulev2 = float,class:^(kitty)$,title:^(kitty)$
  #    # See https://wiki.hyprland.org/Configuring/Window-Rules/ for more

  #    windowrulev2 = workspace name:$ws1,fullscreen,class:^(firefox|Chromium)$
  #    # make firefox share indicator float
  #    windowrulev2 = float,nofullscreen,class:firefox,title:^Firefox — Sharing Indicator$

  #    windowrulev2 = workspace name:$ws2,class:^(xterm|urxvt|aterm|URxvt|XTerm|Alacritty|Emacs|foot)$
  #    windowrulev2 = workspace name:$ws3,class:^(Kopete|Pidgin|gajim|rambox|Dino|.gam-wrapped|Daily|birdie|evolution|Ferdi|Ferdium)$
  #    windowrulev2 = workspace name:$ws4,class:^(signal)$
  #    windowrulev2 = workspace name:$ws5,class:^(Evince|GVim|keepassx|libreoffice)$

  #    # See https://wiki.hyprland.org/Configuring/Keywords/ for more
  #    $mainMod = SUPER

  #    # Example binds, see https://wiki.hyprland.org/Configuring/Binds/ for more
  #    bind = $mainMod, Q, exec, footclient
  #    bind = $mainMod, C, killactive,
  #    bind = $mainMod, M, exit,
  #    #bind = $mainMod, E, exec, dolphin
  #    bind = $mainMod, V, togglefloating,
  #    bind = $mainMod, F, fullscreen, 1
  #    bind = $mainMod, R, exec, wofi --show run --gtk-dark
  #    bind = $mainMod, P, pseudo, # dwindle
  #    bind = $mainMod, J, togglesplit, # dwindle

  #    # Move focus with mainMod + arrow keys
  #    bind = $mainMod, left, movefocus, l
  #    bind = $mainMod, right, movefocus, r
  #    bind = $mainMod, up, movefocus, u
  #    bind = $mainMod, down, movefocus, d

  #    # Switch workspaces with mainMod + [0-9]
  #    bind = $mainMod, 1, workspace, name:$ws1
  #    bind = $mainMod, 2, workspace, name:$ws2
  #    bind = $mainMod, 3, workspace, name:$ws3
  #    bind = $mainMod, 4, workspace, name:$ws4
  #    bind = $mainMod, 5, workspace, name:$ws5
  #    bind = $mainMod, 6, workspace, name:$ws6
  #    bind = $mainMod, 7, workspace, name:$ws7
  #    bind = $mainMod, 8, workspace, name:$ws8
  #    bind = $mainMod, 9, workspace, name:$ws9
  #    bind = $mainMod, 0, workspace, name:$ws0

  #    # Move active window to a workspace with mainMod + SHIFT + [0-9]
  #    bind = $mainMod SHIFT, 1, movetoworkspace, name:$ws1
  #    bind = $mainMod SHIFT, 2, movetoworkspace, name:$ws2
  #    bind = $mainMod SHIFT, 3, movetoworkspace, name:$ws3
  #    bind = $mainMod SHIFT, 4, movetoworkspace, name:$ws4
  #    bind = $mainMod SHIFT, 5, movetoworkspace, name:$ws5
  #    bind = $mainMod SHIFT, 6, movetoworkspace, name:$ws6
  #    bind = $mainMod SHIFT, 7, movetoworkspace, name:$ws7
  #    bind = $mainMod SHIFT, 8, movetoworkspace, name:$ws8
  #    bind = $mainMod SHIFT, 9, movetoworkspace, name:$ws9
  #    bind = $mainMod SHIFT, 0, movetoworkspace, name:$ws0

  #    # Scroll through existing workspaces with mainMod + scroll
  #    bind = $mainMod, mouse_down, workspace, e+1
  #    bind = $mainMod, mouse_up, workspace, e-1

  #    # Move/resize windows with mainMod + LMB/RMB and dragging
  #    bindm = $mainMod, mouse:272, movewindow
  #    bindm = $mainMod, mouse:273, resizewindow
  #  '';
  #};
}
