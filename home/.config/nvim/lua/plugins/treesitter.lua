return {
  "nvim-treesitter/nvim-treesitter",
  build = "true",
  opts = function(_, opts)
    vim.fn.system(
      "ln -sf ${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site/parser/*.so ${XDG_DATA_HOME:-$HOME/.local/share}/nvim/lazy/nvim-treesitter/parser"
    )
  end,
}
