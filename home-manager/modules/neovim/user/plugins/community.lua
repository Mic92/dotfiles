return {
  -- Add the community repository of plugin specifications
  "AstroNvim/astrocommunity",
  -- example of imporing a plugin, comment out to use it or add your own
  -- available plugins can be found at https://github.com/AstroNvim/astrocommunity
 --{ import = "astrocommunity.colorscheme.catppuccin" },
 --{ "catppuccin",
 --  opts = {
 --    flavour = "mocha",
 --  }
 --},
 { import = "astrocommunity.colorscheme.gruvbox" },
 { import = "astrocommunity.completion.copilot-lua" },
 { -- further customize the options set by the community
    "copilot.lua",
    opts = {
      suggestion = {
        keymap = {
          accept = "<C-l>",
          accept_word = false,
          accept_line = false,
          next = "<C-.>",
          prev = "<C-,>",
          dismiss = "<C/>",
        },
      },
    },
  },
  { import = "astrocommunity.completion.copilot-lua-cmp" },
}
