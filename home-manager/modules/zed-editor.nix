{
  programs.zed-editor = {
    enable = true;
    # https://github.com/zed-industries/extensions/tree/main/extensions
    extensions = [
      "git-firefly"
      "html"
      "nix"
      "terraform"
      "toml"
    ];
    userSettings = {
      agent = {
        always_allow_tool_actions = true;
        inline_assistant_model = {
          provider = "copilot_chat";
          model = "claude-sonnet-4-latest";
        };
        default_model = {
          provider = "zed.dev";
          model = "claude-sonnet-4-latest";
        };
        version = "2";
      };
      autosave = "on_focus_change";
      features = {
        edit_prediction_provider = "zed";
      };
      ssh_connections = [
        {
          host = "eve.i";
          projects = [
            {
              paths = ["~/git"];
            }
          ];
        }
        {
          host = "eve.i";
          projects = [];
        }
      ];
      ui_font_size = 13;
      buffer_font_size = 13;
      buffer_font_family = "Hack";
      ui_font_family = "Hack";
      theme = "Solarized Light";
      buffer_font_fallbacks = ["Noto Sans"];
      vim_mode = true;
    };
  };
}
