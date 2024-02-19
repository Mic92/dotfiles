return {
 -- Add the community repository of plugin specifications
 "AstroNvim/astrocommunity",
 -- example of imporing a plugin, comment out to use it or add your own
 -- available plugins can be found at https://github.com/AstroNvim/astrocommunity
 { import = "astrocommunity.completion.copilot-lua" },
 { import = "astrocommunity.completion.copilot-lua-cmp" },
 {
  "zbirenbaum/copilot.lua",
  opts = {
   filetypes = {
    gitcommit = true,
   },
  },
 },
 { import = "astrocommunity.project.project-nvim" },
 { import = "astrocommunity.pack.go" },
 {
  "ray-x/go.nvim",
  -- don't install go binaries with the plugin
  -- instead we install these with nix: https://github.com/ray-x/go.nvim#go-binaries-install-and-update
  build = "true",
 },
 { import = "astrocommunity.pack.bash" },
 { import = "astrocommunity.pack.python" },
 { import = "astrocommunity.pack.lua" },
 { import = "astrocommunity.pack.toml" },
 { import = "astrocommunity.pack.markdown" },
 { import = "astrocommunity.pack.rust" },
 { import = "astrocommunity.pack.yaml" },
 { import = "astrocommunity.pack.nix" },
 { import = "astrocommunity.editing-support.rainbow-delimiters-nvim" },
 { import = "astrocommunity.editing-support.auto-save-nvim" },
 {
  "Tsuzat/NeoSolarized.nvim",
  name = "NeoSolarized.nvim",
  opts = {
    style = "light", -- "dark" or "light"
    transparent = false,
  },
 },
}
