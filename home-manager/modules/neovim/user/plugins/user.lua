return {
  {
    "lewis6991/spaceless.nvim",
    init = function() require("spaceless").setup() end,
  },
  {
    "ziontee113/icon-picker.nvim",
    init = function()
      require("icon-picker").setup({ disable_legacy_commands = true})
    end,
  },
}
